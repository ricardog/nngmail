
import base64
from copy import deepcopy
import logging
import pdb
import queue
import threading

import click
from flask.logging import default_handler
import httplib2
from tqdm import tqdm

from . import local
from . import remote

class RequestFormatter(logging.Formatter):
    pass

formatter = logging.Formatter(
    '[%(asctime)s] %(levelname)s nnsync: %(message)s'
)
default_handler.setFormatter(formatter)

logger = logging.getLogger('nnsync')
logger.setLevel(logging.INFO)
logger.addHandler(default_handler)

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
    """A synchronization object for keeping a local copy of message metadata in a local storage.

This object represents only one Gmail account.  When synchronizng
multiple accounts, need to create one object per account.

    """
    def __init__(self, email, nickname, opts):
        self.email = email
        self.nickname = nickname
        self.opts = deepcopy(opts)
        gmail_opts = self.opts.get('gmail', {})
        gmail_opts.update({'email': self.email})
        self.gmail = remote.Gmail(**gmail_opts)
        local_opts = self.opts.get('local', {})
        local_opts.update({'email': self.email,
                           'nickname': self.nickname,
                           'writable': self.gmail.writable,
                           'can_send': self.gmail.can_send})
        self.sql3 = local.Sqlite3(**local_opts)
        if self.opts['verbose']:
            self.bar = tqdm
        else:
            self.bar = _tqdm

    @classmethod
    def from_account(cls, account, config):
        """Create a synchronization object from an Account object.

This is useful for creating a sync object from the Flask controllers."""
        return cls(account.email, account.nickname, config)

    def sync_labels(self):
        """Synchronize Gmail labels with database."""
        for label in self.gmail.get_labels():
            self.sql3.new_label(name=label['name'], gid=label['id'])

    def read(self, ids):
        """Read messages from the database.

If the message body is not available, fetch it from Gmail and store it
in the database.  The messages to read are specified in ids.  Note that
this function does not return the message data.  Rather the data is
store in the database from where it can be accessed via the usual
mechanisms.

        """
        if not ids:
            return
        if '__getitem__' not in dir(ids):
            ids = (ids, )
        msgs = self.sql3.find(ids)
        id_map = dict(((m.google_id, m) for m in msgs))
        assert len(msgs) == len(ids)
        needed = tuple(map(lambda m: m.google_id,
                           filter(lambda m: m.raw is None, msgs)))
        if self.gmail.reachable():
            logger.info('%s: reading %d messages' % (self.nickname, len(needed)))
            bar = self.bar(leave=True, total=len(needed),
                           desc="caching messages")
            for batch in self.gmail.get_messages(needed, format='raw'):
                for msg in batch:
                    blob = base64.b64decode(msg['raw'], altchars='-_')
                    id_map[msg['id']].raw = blob
                    bar.update(1)
            # Store raw message data fetch during read
            self.sql3.commit()
            bar.close()

    def create_or_update(self, gids, create=True, sync_labels=False):
        """Create or update message metadata in local storage.

This function provides a wrapper to iterate through a series of messages
to either create them or update them in local storage.  Splits the list
of Google ID's to operate on, and retrieve the message metadata in
batches.  Do one database commit per batch for efficiency.

In this context create or update refers to message metadata only.
Fetching the message itself is done via a different API.

If sync_labels is True, then sync labels before proceeding--usually a
good idea since we create / update messages in batches and label
synchronization is fast.

Messages are identified by google ID's

        """
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
        """Wrapper for creating messages.

Because we use a composite primary key (id, account_id) for the message
table, we first create a placeholder for each message.  This quick and
is done as a transaction so we only need to read the current id once at
the start.  Once the placeholder was created process each message.

        """
        return self.create_or_update(gids, True, sync_labels)

    def update(self, gids, sync_labels=False):
        """Wrapper for updating message metadata.

This is needed when the user interrupts a synchronization operation and
then restarts it in a different process.

        """
        return self.create_or_update(gids, False, sync_labels)

    def update_labels(self, updated):
        """Update message labels.

Message metadata never changes and doesn't need to be updated.  But
labels do change.

        """

        # updated is a hash of the form
        # {'163e42f92b6526f8': ['IMPORTANT', 'STARRED', 'CATEGORY_UPDATES',
        #                       'INBOX']}
        # So convert to pseudo messages
        msgs = [{'id': k, 'labelIds': v} for k, v in updated.items()]
        self.sql3.update(msgs)

    def delete(self, gids):
        """Delete a message from local storage."""
        self.sql3.delete(gids)

    def remote_batch_update(self, ids, add_labels, rm_labels):
        """Update a set of message in Gmail.

Only label updates are possible.

        """
        if not self.gmail.writable:
            return None
        return self.gmail.update_messages(ids, add_labels, rm_labels)

    def remote_update(self, id, labels):
        """Update a single message at the remote server."""
        if not self.gmail.writable:
            return None
        return self.gmail.update_message(id, labels)

    def remote_delete(self, id):
        """Delete a message at the remote."""
        if not self.gmail.writable:
            return None
        return self.gmail.delete_message(id)

    def get_history(self):
        """Request a of changes for the account since we last synchronized.

This is a faster way to synchronize that doing a full pull of the
account and is valid so long as we sycnhronize regularly (Gmail docs say
the history list should be valid for about a week).

        """
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
        """Merge items in the history list.

For example, if a label is added and then removed from a message, we can
skip both operations.  Operations on any deleted messages are also
skipped.

        """
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
        """Do an incremental update of the account.

If Gmail cannot provide us with a lis of changes, either because we have
never synchronized this account or because we haven't synchronized it
recently, do a full update.

        """
        if not self.gmail.reachable():
            return

        self.sync_labels()

        if self.sql3.get_history_id() == 0:
            history = None
        else:
            history = self.get_history()
        if history is None:
            logger.info('No history available; attempting full pull')
            self.full_pull()
            return

        if len(history) == 0:
            logger.info('%s: no new changes' % self.nickname)
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
        #self.sql3.set_history_id(hid)
        logger.info('%s: new historyId: %d' % (self.nickname, hid))
        self.read(self.sql3.gid_to_id(tuple(added.keys())))

    def full_pull(self):
        """Do a full update on the account.

If a message (based on the Google ID) is already present in the local
database, then only update the labels.

        """
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

        ## Created here means they need a placeholder.  Assume any
        ## messages already in the DB came from a previous (interrupted)
        ## import.
        created = sorted(created, key=lambda a: int(a, 16))
        updated = sorted(updated, key=lambda a: int(a, 16))
        self.sql3.placeholder(created)
        hid1 = self.create(created + updated)
        self.delete(local_gids)

        history_id = max(hid1, history_id)
        logger.info('new historyId: %d' % history_id)
        self.sql3.set_history_id(history_id)

    def init_cache(self):
        """Fetch cacheable messages."""
        ids = self.sql3.find_cacheable()
        self.read(ids)

    def sync(self):
        """Create a background thread to poll for changes.

This function creates a thread that does two things:

    - Listen for commands on a queue
    - Periodically polls for changes in the account

The command queue is meant to provide a backfround job processing
mechanism for the front-end server.  It could, for example, be asked to
prefetch all messages in a thread when accessing any of the messages in
the thread.

The poll timeout is implemented as timeout on the (clocking) dequeue of
commands.

        """
        def __sync(email, nickname, opts, ingress, egress):
            me = NnSync(email, nickname, opts)
            logger.info("%s: start sync" % me.nickname)
            while True:
                try:
                    data = ingress.get(block=True,
                                       timeout=me.gmail.poll_interval)
                    if not data:
                        ingress.task_done()
                        break
                    # Process cmd
                    cmd, args = data
                    logger.info("%s: received cmd %s" % (me.nickname, cmd))
                    if cmd == 'read':
                        me.read(args)
                    else:
                        logger.error('%s: unknown command %s' %
                                     (me.nickname, cmd))
                    ingress.task_done()
                    egress.put(cmd)
                except queue.Empty:
                    ## we were just woken up.  Do nothing here, go pull
                    ## from gmail.
                    pass
                logger.info('%s: pull' % me.nickname)
                try:
                    me.pull()
                except (ConnectionResetError,
                        httplib2.ServerNotFoundError,
                        TimeoutError):
                    ## Likely the connection died while talking to
                    ## or trying to establish a connection with the
                    ## server.  Go back to sleep and hope we have
                    ## better luck next time.
                    pass
            logger.info("%s: stop sync" % me.nickname)

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
        """Interface for passing a search string to the Gmail back end."""
        return self.gmail.search(query, labels)

    def expire_cache(self):
        """Function to periodically (once a day) expire cached messages."""
        self.sql3.expire_cache()
