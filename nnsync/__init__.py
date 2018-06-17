
import base64
import click
from copy import deepcopy
import queue
import threading
from tqdm import tqdm

from . import local
from . import remote

import pdb

class _tqdm(tqdm):
    """Dummy version of tqdm that does not print."""
    @staticmethod
    def status_printer(file):
        def print_status(s):
            return
        return print_status
    def close(self):
        return
    def update(self, n=1):
        return

class NnSync():
    def __init__(self, email, nickname, opts):
        self.email = email
        self.nickname = nickname
        self.opts = deepcopy(opts)
        local_opts = self.opts.get('local', {})
        local_opts.update({'email': self.email,
                           'nickname': self.nickname})
        gmail_opts = self.opts.get('gmail', {})
        gmail_opts.update({'email': self.email})
        self.sql3 = local.Sqlite3(**local_opts)
        self.gmail = remote.Gmail(**gmail_opts)
        if self.opts['verbose']:
            self.bar = tqdm
        else:
            self.bar = _tqdm

    @classmethod
    def from_account(cls, account, config):
        return cls(account.email, account.nickname, config)

    def sync_labels(self):
        for label in self.gmail.get_labels():
            self.sql3.new_label(name=label['name'], gid=label['id'])

    def read(self, ids):
        if not ids:
            return
        if '__getitem__' not in dir(ids):
            ids = (ids, )
        msgs = self.sql3.find(ids)
        id_map = dict(((m.google_id, m) for m in msgs))
        assert len(msgs) == len(ids)
        needed = tuple(map(lambda m: m.google_id,
                           filter(lambda m: m.raw is None, msgs)))
        bar = self.bar(leave=True, total=len(needed), desc="caching messages")
        for batch in self.gmail.get_messages(needed, format='raw'):
            for msg in batch:
                blob = base64.b64decode(msg['raw'], altchars='-_')
                id_map[msg['id']].raw = blob
                bar.update(1)
        # Store raw message data fetch during read
        self.sql3.commit()
        bar.close()

    def create_or_update(self, gids, create=True, sync_labels=False):
        history_id = self.sql3.get_history_id()
        if not gids:
            return history_id
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        if sync_labels:
            self.sync_labels()
        bar = self.bar(leave=True, total=len(gids), desc='fetching metadata')
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
        # updated is a hash of the form
        # {'163e42f92b6526f8': ['IMPORTANT', 'STARRED', 'CATEGORY_UPDATES',
        #                       'INBOX']}
        # So convert to pseudo messages
        msgs = [{'id': k, 'labelIds': v} for k, v in updated.items()]
        self.sql3.update(msgs)

    def delete(self, gids):
        self.sql3.delete(gids)

    def get_history(self):
        history = []
        no_history = False
        history_id = self.sql3.get_history_id()

        bar = self.bar(leave=True, total=10, desc='fetching changes')
        pages = 0
        try:
            for results in self.gmail.get_history_since(history_id):
                history += results
                pages += 1
                bar.update(1)
        except remote.Gmail.NoHistoryException:
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

        bar = self.bar(leave=True, total=len(history) - 1,
                       desc='merging changes')
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
            bar.update(1)
        bar.close()
        return(int(history[-1]['id']), deleted, added, updated)

    def pull(self):
        if not self.gmail.reachable():
            return

        self.sync_labels()

        if self.sql3.get_history_id() == 0:
            history = None
        else:
            history = self.get_history()
        if history is None:
            click.echo('No history available; attempting full pull')
            self.full_pull()
            return

        if len(history) == 0:
            click.echo('no new changes')
            return

        hid, deleted, added, updated = self.merge_history(history)
        total = len(deleted) + len(added) + len(updated)
        bar = self.bar(leave=True, total=total, desc='applying changes')
        self.delete(tuple(deleted.keys()))
        bar.update(len(deleted))
        self.create(tuple(added.keys()))
        bar.update(len(added))
        self.update_labels(updated)
        bar.update(len(updated.keys()))
        bar.close()
        self.sql3.set_history_id(hid)
        click.echo('new historyId: %d' % hid)
        self.read(added.keys())

    def full_pull(self):
        history_id = 0
        local_gids = set(self.sql3.all_ids())
        created = []
        updated = []

        prof = self.gmail.get_profile()
        bar = self.bar(leave=True, total=prof['messagesTotal'],
                       desc="fetching message ID's")
        for (_, msgs) in self.gmail.list_messages(limit=None):
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
            click.echo("WARN: creating id's out of order! (%s %s)" %
                       (created[0], updated[-1]))
        hid1 = self.create(created)
        hid2 = self.update(updated)
        self.delete(local_gids)

        history_id = max(hid1, hid2, history_id)
        click.echo('new historyId: %d' % history_id)
        self.sql3.set_history_id(history_id)

    def init_cache(self):
        ids = self.sql3.find_cacheable()
        self.read(ids)

    def sync(self):
        def __sync(email, nickname, opts, ingress, egress):
            me = NnSync(email, nickname, opts)
            click.echo("%s: start sync" % me.nickname)
            while True:
                try:
                    data = ingress.get(block=True,
                                       timeout=me.gmail.poll_interval)
                    if not data:
                        ingress.task_done()
                        break
                    # Process cmd
                    cmd, args = data
                    click.echo("%s: received cmd %s" % (me.nickname, cmd))
                    if cmd == 'read':
                        me.read(args)
                    else:
                        click.echo('%s: unknown command %s' %
                                   me.nickname, cmd)
                    ingress.task_done()
                    egress.put(cmd)
                except queue.Empty:
                    click.echo('%s: pull' % me.nickname)
                    me.pull()
            click.echo("%s: stop sync" % me.nickname)

        ingress = queue.Queue()
        egress = queue.Queue()
        thread = threading.Thread(daemon=True,
                                  target=lambda: __sync(self.email,
                                                        self.nickname,
                                                        self.opts,
                                                        ingress, egress))
        thread.start()
        return (thread, ingress, egress)

    def search(self, query, labels=[]):
        return self.gmail.search(query, labels)
