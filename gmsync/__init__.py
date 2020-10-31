
import base64
from copy import deepcopy
import logging
import queue
import socket
import threading

from flask.logging import default_handler
import googleapiclient
import httplib2
import sqlite3
from tqdm import tqdm

from . import local
from . import remote

class RequestFormatter(logging.Formatter):
    pass

formatter = logging.Formatter(
    '[%(asctime)s] %(levelname)s gmsync: %(message)s'
)
default_handler.setFormatter(formatter)

logger = logging.getLogger('gmsync')
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

class GmSync():
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
            logger.info('%s: reading %d messages' % (self.nickname,
                                                     len(needed)))
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

    def create(self, gids, sync_labels=False):
        """Create message metadata in local storage.

This function provides a wrapper to iterate through a series of messages
to create them in local storage.  It splits the list of Google ID's to
operate on, and retrieves the message metadata in batches.  It does one
database commit per batch for efficiency.

In this context create refers to message metadata only.  Fetching the
message itself is done via a different API.

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
        for msgs in self.gmail.get_messages(gids, 'metadata'):
            self.sql3.create(msgs)
            bar.update(len(msgs))
            if msgs:
                history_id = max(history_id,
                                 max([int(msg['historyId']) for msg in msgs]))
        bar.close()
        return history_id

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

    def remote_batch_update(self, gids, add_labels, rm_labels):
        """Update a set of message in Gmail.

Only label updates are possible.

        """
        if not self.gmail.writable:
            return None
        return self.gmail.update_messages(gids, add_labels, rm_labels)

    def remote_update(self, id, labels):
        """Update a single message at the remote server."""
        if not self.gmail.writable:
            return None
        return self.gmail.update_message(id, labels)

    def trash(self, msg):
        """Move a message to the trash."""
        if not self.gmail.writable:
            return None
        rval = self.gmail.trash(msg.google_id)
        #rval = None
        if rval:
            # Remote trash failed so skip local trash
            return rval
        return self.sql3.trash(msg)

    def untrash(self, msg):
        """Move a message from the trash."""
        if not self.gmail.writable:
            return None
        rval = self.gmail.untrash(msg.google_id)
        #rval = None
        if rval:
            # Remote trash failed so skip local trash
            return rval
        return self.sql3.untrash(msg)

    def get_history(self):
        """Request a list of changes since last synch for the account.

This is a faster way to synchronize than doing a full pull of the
account and is valid so long as we synchronized recently (Gmail docs say
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
                            updated[msg['id']] = msg.get('labelIds', [])
            bar.update(1)
        bar.close()
        return(int(history[-1]['id']), deleted, added, updated)

    def verify(self):
        """Verify messages received in the last month.

Verifies that all messages received in the past week have the correct
properties in the local database.

        """
        if not self.opts.get('verify', True):
            return
        all_ids = []
        for _, msgs in self.gmail.list_messages(limit=None,
                                                query='newer_than:7d'):
            all_ids += [m['id'] for m in msgs]
        lmsgs = self.sql3.find_by_gid(all_ids)
        if lmsgs:
            lhash = dict((lm.google_id, lm) for lm in lmsgs)
            if len(lmsgs) != len(all_ids):
                print('ERROR: verification failed')
                print('\tlen(msgs) != len(all_ids) --'
                      f'{len(msgs)} != {len(all_ids)}')
                import os; os._exit(-1)
        else:
            assert len(all_ids) == 0
        bar = self.bar(leave=True, total=len(all_ids), desc='verify')
        for gid in all_ids:
            try: 
                rmsg = self.gmail.get_message(gid, 'metadata')
                lmsg = lhash[gid]
                if (set([lm.gid for lm in lmsg.labels]) !=
                    set(rmsg.get('labelIds', []))):
                    print('verify failed: %d: %s: %s' % (lmsg.id, lmsg.google_id,
                                                         lmsg.subject))
                    print('  local : ', ', '.join(sorted([lm.gid for lm in lmsg.labels])))
                    print('  remote: ', ', '.join(sorted(rmsg['labelIds'])))
            except googleapiclient.errors.HttpError as ex:
                if ex.resp.status == 404:
                    # FIXME: Why am I getting 404?  Message deleted
                    # between list and get?
                    logger.info('Verify failed to fetch message')
                    pass
            bar.update(1)
        bar.close()
        ## FIXME: verify there local DB doesn't have extra messages
        return


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

        self.sql3.placeholder(tuple(added.keys()), skip_ok=True)
        self.create(tuple(added.keys()))
        bar.update(len(added))

        self.update_labels(updated)
        bar.update(len(updated.keys()))

        bar.close()
        #self.sql3.set_history_id(hid)
        logger.info('%s: new historyId: %d' % (self.nickname, hid))
        self.sql3.set_history_id(hid)
        self.read(self.sql3.gid_to_id(tuple(added.keys())))
        logger.info('%s: sync complete' % self.nickname)
        self.verify()


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
        ## messages already in the DB came from a previous (interrupted
        ## or older) import.
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

The command queue is meant to provide a background job processing
mechanism for the front-end server.  It could, for example, be asked to
prefetch all messages in a thread when accessing any of the messages in
the thread.

The poll timeout is implemented as timeout on the (clocking) dequeue of
commands.

        """
        def __sync(email, nickname, opts, ingress, egress):
            first = True
            me = GmSync(email, nickname, opts)
            logger.info("%s: start sync" % me.nickname)
            while True:
                if first:
                    timeout = 0
                    first = False
                else:
                    timeout = me.gmail.poll_interval
                try:
                    data = ingress.get(block=True,
                                       timeout=timeout)
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
                        TimeoutError, socket.timeout,
                        OSError,
                        sqlite3.OperationalError) as ex:
                    ## Likely the connection died while talking to
                    ## or trying to establish a connection with the
                    ## server.  Go back to sleep and hope we have
                    ## better luck next time.
                    logger.info('%s: connection error: abort pull: %s' %
                                (me.nickname, ex))
                    me.gmail.reset_http()
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
