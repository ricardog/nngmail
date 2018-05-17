#!/usr/bin/env python3

import base64
from collections import Iterable
import logging

from tqdm import tqdm

import local2
import remote

import pdb

class NnGmail():
    def __init__(self, opts):
        self.db_file = opts['db_file']
        self.max_payload = opts['max_payload']
        self.cache_lifetime = opts['cache_lifetime']
        self.email = opts['email']
        self.sql3 = local2.Sqlite3(self.db_file)
        self.gmail = remote.Gmail()

    def sync_labels(self):
        for label in self.gmail.get_labels():
            self.sql3.new_label(name=label['name'], gid=label['id'])

    def sync_history_id(self):
        history_id = self.sql3.get_history_id()
        self.sql3.set_history_id(self.gmail.get_history_id(history_id))
        
    def create(self, gids, sync_labels=False):
        history_id = 0
        if not gids:
            return self.sql3.get_history_id()
        if sync_labels:
            self.sync_labels()
        if isinstance(gids, str) or not isinstance(gids, Iterable):
            msg = self.gmail.get_message(gids, 'metadata')
            self.sql3.create(msg['id'], msg['threadId'], msg['labelIds'],
                             int(msg['internalDate']) / 1000,
                             msg['sizeEstimate'],
                             msg['payload']['headers'], msg['snippet'])
            history_id = max(history_id, int(msg['historyId']))
        else:
            bar = tqdm(leave=True, total=len(gids), desc='fetching metadata')
            results = self.gmail.get_messages(gids, 'metadata')
            for batch in results:
                for msg in sorted(batch, key=lambda m: int(m['internalDate'])):
                    self.sql3.create(msg['id'], msg['threadId'],
                                     msg.get('labelIds', []),
                                     int(msg['internalDate']) / 1000,
                                     msg['sizeEstimate'],
                                     msg['payload']['headers'], msg['snippet'],
                                     False)
                    bar.update(1)
                    history_id = max(history_id, int(msg['historyId']))
                self.sql3.commit()
            bar.close()
        return history_id

    def cacheable(self, msg):
        return True

    def read(self, ids):
        msgs = self.sql3.find(ids)
        id_map = dict(((m.google_id, m) for m in msgs))
        assert len(msgs) == len(ids)
        needed = tuple(map(lambda m: m.google_id,
                           filter(lambda m: m.raw is None, msgs)))
        raw = {}
        for batch in self.gmail.get_messages(needed, format='raw'):
            for msg in batch:
                blob = base64.b64decode(msg['raw'], altchars='-_')
                id_map[msg['id']].raw = blob
        # Store raw message data fetch during read
        self.sql3.commit()
        return [raw[msg.id] if msg.id in raw else msg.raw for msg in msgs]


    def update(self, gids, sync_labels=False):
        history_id = 0
        if not gids:
            return self.sql3.get_history_id()
        if sync_labels:
            self.sync_labels()
        history_id = 0
        if isinstance(gids, str) or not isinstance(gids, Iterable):
            msg = self.gmail.get_message(gid, 'metadata')
            self.sql3.update(msg['id'], msg['labelIds'])
            history_id = max(history_id, int(msg['historyId']))
        else:
            bar = tqdm(leave=True, total=len(gids), desc='updating messages')
            results = self.gmail.get_messages(gids, 'metadata')
            for batch in results:
                for msg in batch:
                    self.sql3.update(msg['id'], msg.get('labelIds', []))
                    history_id = max(history_id, int(msg['historyId']))
                    bar.update(1)
                self.sql3.session.flush()
            bar.close()
        return history_id

    def update2(self, updated):
        for id in updated.keys():
            self.sql3.update(id, updated[id])
        self.sql3.session.flush()

    def delete(self, gids):
        self.sql3.delete(gids)

    def get_history(self):
        history = []
        no_history = False
        history_id = self.sql3.get_history_id()
        
        bar = tqdm(leave=True, total=10, desc='fetching changes')
        pages = 0
        try:
            for results in self.gmail.get_history_since(history_id):
                history += results
                pages += 1
                bar.update(1)
        except remote.Gmail.NoHistoryException as ex:
            no_history = True
        for _ in range(pages, 10):
            bar.update(1)
        bar.close()
        if no_history:
            return None
        return history
        
    def merge_history(self, history):
        deleted = {}
        added = {}
        updated = {}
        
        bar = tqdm(leave=True, total=len(history) - 1, desc='merging changes')
        for item in map(lambda i: history[i],
                        range(len(history) - 1, -1, -1)):
            if 'messagesDeleted' in item:
                for msg in map(lambda m: m['message'],
                               item['messagesDeleted']):
                    deleted[msg['id']] = True
            if 'messagesAdded' in item:
                for msg in filter(lambda m: m['id'] not in deleted,
                                  map(lambda m: m['message'],
                                      item['messagesAdded'])):
                    added[msg['id']] = msg
            for kind in ('labelsAdded', 'labelsRemoved'):
                if kind in item:
                    for msg in filter(lambda m: (m['id'] not in deleted and
                                                 m['id'] not in added),
                                      map(lambda m: m['message'],
                                          item[kind])):
                        if msg['id'] not in updated:
                            updated[msg['id']] = msg['labelIds']
            bar.update()
        bar.close()
        return(int(history[-1]['id']), deleted, added, updated)

    def pull(self):
        self.sync_labels()

        if self.sql3.get_history_id() == 0:
            history = None
        else:
            history = self.get_history()
        if history is None:
            print('No history available; attempting full pull')
            self.full_pull()
            return

        if len(history) == 0:
            print('no new changes')
            return

        hid, deleted, added, updated = self.merge_history(history)
        total = len(deleted) + len(added) + len(updated)
        bar = tqdm(leave=True, total=total, desc='applying changes')
        self.delete(tuple(deleted.keys()))
        bar.update(len(deleted))
        self.create(tuple(added.keys()))
        bar.update(len(added))
        self.update2(updated)
        bar.close()
        self.sql3.set_history_id(hid)
                  
    def full_pull(self):
        total = 1
        history_id = 0
        local_gids = set(self.sql3.all_ids())
        created = []
        updated = []

        print('counting messages', end='\r')
        for (total, msgs) in self.gmail.list_messages(limit=None):
            gids = set([msg['id'] for msg in msgs])
            created.extend(gids - local_gids)
            updated.extend(local_gids.intersection(gids))
            local_gids = local_gids - gids
        print('')
        
        created = sorted(created, key=lambda a: int(a, 16))
        hid1 = self.create(created)
        hid2 = self.update(updated)
        self.delete(local_gids)

        history_id = max(hid1, hid2, history_id)
        print('new historyId: %d' % history_id)
        self.sql3.set_history_id(history_id)
        
def main():
    if None:
        logging.basicConfig(filename='sql.log')
        logger = logging.getLogger('sqlalchemy.engine')
        logger.setLevel(logging.DEBUG)

    nngmail = NnGmail({'db_file': 'nngmail.sqlite3',
                       'email': 'ricardog@siliconartisans.com',
                       'max_payload': 1024*1024,
                       'cache_lifetime': 31})
    nngmail.pull()
    msgs = nngmail.read(range(2140, 2150))
    #print(msgs)
    
if __name__ == '__main__':
    main()
