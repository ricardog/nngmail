#!/usr/bin/env python3

from collections import Iterable
import logging

from tqdm import tqdm

import local2
import remote

import pdb

class NnGmail():
    def __init__(self, opts):
        self.db_file = opts['db_file']
        self.sql3 = local2.Sqlite3(self.db_file)
        self.gmail = remote.Gmail()

    def sync_labels(self):
        for label in self.gmail.get_labels():
            self.sql3.new_label(name=label['name'], gid=label['id'])

    def sync_history_id(self):
        history_id = self.sql3.get_history_id()
        self.sql3.set_history_id(self.gmail.get_history_id(history_id))
        
    def store(self, gids, sync_labels=False):
        if not gids:
            return self.sql3.get_history_id()
        if sync_labels:
            self.sync_labels()
        if not isinstance(gids, Iterable):
            msg = self.gmail.get_message(gids, 'metadata')
            self.sql3.store(msg['id'], msg['threadId'], msg['labelIds'],
                            int(msg['internalDate']) / 1000,
                            msg['sizeEstimate'],
                            msg['payload']['headers'], msg['snippet'])
            history_id = int(msg['historyId'])
        else:
            results = self.gmail.get_messages(gids, 'metadata')
            for batch in results:
                for msg in batch:
                    if msg['id'] in('15c677afd6ef4607', '1574e5d1d2c37ffa'):
                        pdb.set_trace()
                    self.sql3.store(msg['id'], msg['threadId'],
                                    msg.get('labelIds', []),
                                    int(msg['internalDate']) / 1000,
                                    msg['sizeEstimate'],
                                    msg['payload']['headers'], msg['snippet'],
                                    False)
                history_id = int(batch[-1]['historyId'])
            self.sql3.commit()
        return history_id

    def update(self, gids, sync_labels=False):
        if not gids:
            return self.sql3.get_history_id()
        if sync_labels:
            self.sync_labels()
        history_id = 0
        if not isinstance(gids, Iterable):
            msg = self.gmail.get_message(gid, 'metadata')
            self.sql3.update(msg['id'], msg['labelIds'])
            history_id = int(msg['historyId'])
        else:
            results = self.gmail.get_messages(gids, 'metadata')
            for batch in results:
                for msg in batch:
                    self.sql3.update(msg['id'], msg.get('labelIds', []))
                history_id = int(batch[-1]['historyId'])
            self.sql3.session.flush()
        return history_id

    def delete(self, gids):
        self.sql3.delete(gids)

    def pull(self):
        total = 1
        bar = tqdm(leave=True, total=total, desc='fetching changes')
        history_id = self.sql3.get_history_id()
        self.sync_labels()

        try:
            for results in self.gmail.get_history_since(history_id):
                pdb.set_trace()
                continue
                (total, msgs) = results
                gids = set([msg['id'] for msg in msgs])
                bar.total = total
                hid1 = self.store(gids - local_gids)
                hid2 = self.update(local_gids.intersection(gids))
                history_id = max(hid1, hid2, history_id)
                local_gids = local_gids - gids
                bar.update(len(msgs))
            self.sql3.set_history_id(history_id)
        except remote.Gmail.NoHistoryException as ex:
            bar.close()
            print('No history available; attempting full pull')
            self.full_pull()
                  
    def full_pull(self):
        total = 1
        bar = tqdm(leave=True, total=total, desc='fetching metadata')
        history_id = 0
        self.sync_labels()
        local_gids = set(self.sql3.all_ids())
        
        for results in self.gmail.list_messages(limit=None):
            (total, msgs) = results
            gids = set([msg['id'] for msg in msgs])
            bar.total = total
            hid1 = self.store(gids - local_gids)
            hid2 = self.update(local_gids.intersection(gids))
            history_id = max(hid1, hid2, history_id)
            local_gids = local_gids - gids
            bar.update(len(msgs))

        for gid in local_gids:
            self.delete(gid)
        bar.close()
        self.sql3.set_history_id(history_id)
        
def main():
    logging.basicConfig(filename='sql.log')
    logger = logging.getLogger('sqlalchemy.engine')
    logger.setLevel(logging.DEBUG)

    me = NnGmail({'db_file': 'nngmail.sqlite3'})
    me.pull()

if __name__ == '__main__':
    main()
