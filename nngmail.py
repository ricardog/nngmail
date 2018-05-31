#!/usr/bin/env python3

import base64
from collections import Iterable
import logging
import sys
import threading

import yaml
from tqdm import tqdm

import local2
import remote

import pdb

class NnGmail():
    def __init__(self, email, opts):
        self.email = email
        local_opts = opts.get('local', {})
        local_opts.update({'email': self.email})
        gmail_opts = opts.get('gmail', {})
        gmail_opts.update({'email': self.email})
        self.sql3 = local2.Sqlite3(**local_opts)
        self.gmail = remote.Gmail(**gmail_opts)

    def sync_labels(self):
        for label in self.gmail.get_labels():
            self.sql3.new_label(name=label['name'], gid=label['id'])

    def sync_history_id(self):
        history_id = self.sql3.get_history_id()
        self.sql3.set_history_id(self.gmail.get_history_id(history_id))

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

    def create_or_update(self, gids, create=True, sync_labels=False):
        history_id = self.sql3.get_history_id()
        if not gids:
            return history_id
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        if sync_labels:
            self.sync_labels()
        bar = tqdm(leave=True, total=len(gids), desc='fetching metadata')
        for batch in self.gmail.get_messages(gids, 'metadata'):
            msgs = sorted(batch, key=lambda m: int(m['internalDate']))
            if create:
                self.sql3.create(msgs)
            else:
                self.sql3.update(msgs)
            bar.update(len(msgs))
            history_id = max(history_id,
                             max([int(msg['historyId']) for msg in msgs]))
        bar.close()
        return history_id

    def create(self, gids, sync_labels=False):
        return self.create_or_update(gids, True, sync_labels)

    def update(self, gids, sync_labels=False):
        return self.create_or_update(gids, False, sync_labels)

    def update_labels(self, updated):
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
        self.update_labels(updated)
        bar.close()
        self.sql3.set_history_id(hid)

    def full_pull(self):
        history_id = 0
        local_gids = set(self.sql3.all_ids())
        created = []
        updated = []

        prof = self.gmail.get_profile()
        bar = tqdm(leave=True, total=prof['messagesTotal'],
                   desc="fetching message ID's")
        for (total, msgs) in self.gmail.list_messages(limit=None):
            gids = set([msg['id'] for msg in msgs])
            created.extend(gids - local_gids)
            updated.extend(local_gids.intersection(gids))
            local_gids = local_gids - gids
            bar.update(len(msgs))
        bar.close()

        created = sorted(created, key=lambda a: int(a, 16))
        updated = sorted(updated, key=lambda a: int(a, 16))
        if (updated and created and
            int(created[0], 16) < int(updated[-1], 16)):
            print("WARN: creating id's out of order! (%s %s)" % (created[0],
                                                                 updated[-1]))
        hid1 = self.create(created)
        hid2 = self.update(updated)
        self.delete(local_gids)

        history_id = max(hid1, hid2, history_id)
        print('new historyId: %d' % history_id)
        self.sql3.set_history_id(history_id)

def main():
    def test(email, config):
        print('creating object for <%s>' % email)
        nngmail = NnGmail(email, config)
        nngmail.pull()
        msgs = nngmail.read(range(2140, 2150))

        session = nngmail.sql3.new_session()
        msg = nngmail.sql3.find(2140)[0]
        msg.labels = msg.labels[0:1]
        session.commit()
        nngmail.sql3.set_history_id(0)
        
    config = yaml.load(open('config.yaml'))
    if 'log' in config and config['log']['enable']:
        logging.basicConfig(filename=config['log']['filename'])
        logger = logging.getLogger('sqlalchemy.engine')
        level = logging.__getattribute__(config['log']['level'])
        logger.setLevel(level)

    engine, factory, session_mk = local2.Sqlite3.open(config['local']['db_url'])
    accounts = set(local2.Sqlite3.probe(session_mk()) +
                   ['ricardog@itinerisinc.com'])
    threads = map(lambda a: threading.Thread(target=lambda: test(a,
                                                                 config)),
                  accounts)
    tuple(map(lambda t: t.run(), threads))
    tuple(map(lambda t: t.join(), threads))
    

if __name__ == '__main__':
    main()
