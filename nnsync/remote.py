
from __future__ import print_function
from collections import deque
import os
import queue
import socket
import sys
import threading
from urllib.error import URLError
import urllib.parse as urlparse

from apiclient.discovery import build
from apiclient import errors
import googleapiclient
from httplib2 import Http
from oauth2client import file, client, tools
from options import Options

class Gmail:
    """Object for accessing gmail via http API."""
    options = Options(email=None,
                      scopes=['https://www.googleapis.com/auth/gmail.readonly'],
                      client_secret_file='client-secret.json',
                      batch_size=100,
                      credentials_path=None,
                      query='-in:chats',
                      num_workers=0,
                      poll_interval=300)
    service = None

    class GenericException(Exception):
        pass

    class UserRateException(Exception):
        pass

    class BatchException(Exception):
        pass

    class NoHistoryException(Exception):
        pass

    @staticmethod
    def batch_executor(creds, cmds):
        """Execute a batc command and check for errors.

Batch Gmail commands require a callback.  Thus function wraps the call
plus callback into a single synchronous function.  Rather than relying
on callbacks, use threads for parallelism.

    :param cmds list: A list (or other iterable) with a collections of
        commands.  Each command consists of a tuple (google_id,
        command), where command is added to the batch() Gmail client
        api.

    :return: A list of response objects.  Each entry of the list
        corresponds to a callback value.

    :raises: Exceptions on error.

        """
        def handler(rid, resp, ex, responses):
            "Callback invoked by Google API to handled message data."
            def ex_is_error(ex, code):
                "Check if exception is error code 'code'."
                return (isinstance(ex, googleapiclient.errors.HttpError) and
                        ex.resp.status == code)
            if ex is not None:
                if ex_is_error(ex, 404):
                    # message could not be found this is probably a
                    # deleted message, spam or draft message since these
                    # are not included in the messages.get() query by
                    # default.
                    print("remote: could not find remote message: %s!" % rid)
                    return

                elif ex_is_error(ex, 400):
                    # message id invalid, probably caused by stray files
                    # in the mail repo
                    print("remote: message id: %s is invalid! "  % rid)
                    return

                elif ex_is_error(ex, 403):
                    raise Gmail.UserRateException(ex)
                else:
                    raise Gmail.BatchException(ex)
            responses.append(resp)

        http = creds.authorize(Http(timeout=2.0))
        service = build('gmail', 'v1', http=http)
        batch = service.new_batch_http_request()
        responses = []
        for gid, cmd in cmds:
            batch.add(cmd, callback=lambda a, b, c: handler(a, b, c,
                                                            responses),
                      request_id=gid)
        batch.execute(http=http)
        return responses

    @staticmethod
    def worker(my_idx, inq, outq):
        """Enry point for new executor threads.

Downloading (or importing) metadata is limited by the round-trip time to
Gmail if we only use one thread.  This wrapper function makes it
possible to start multiple threads (currently limited to two because
that is how many concurrent requests from the same user Gmail alows) to
reduce the import time.

Commands come in via a thread-safe queue (inq) and response data is
written to another thread-safe queue (outq).  This function does not
interpret the data in either queue.  It merly acts as a dumb pipeline
between the two endpoints.

   :param inq queue.Queue: Inress queue.  Commands received on this
        queue are set to a batch_executor.

   :param outq queue.Queue: Egress queue.  Data returned by the batch
        executor is written to the queue for consumption by the
        initiator.

        """
        print("worker %d starting" % my_idx)
        while True:
            cmd = inq.get()
            if cmd is None:
                break
            ridx, creds, cmds = cmd
            try:
                responses = Gmail.batch_executor(creds, cmds)
            except Exception as ex:
                outq.put([ridx, ex])
            else:
                outq.put([ridx, responses])
            finally:
                inq.task_done()

    def __init__(self, **kwargs):
        """Initialize a new object using the options passed in."""
        self.opts = self.options.push(kwargs)
        data_dir = os.path.normpath(os.path.join(os.path.dirname(__file__),
                                                 '../data'))
        if self.opts.credentials_path is None:
            self.opts.set(credentials_path='%s-creds.json' % self.opts.email)
        if os.path.relpath(self.opts.client_secret_file):
            self.opts.set(client_secret_file=os.path.join(data_dir,
                                                   self.opts.client_secret_file))
        if os.path.relpath(self.opts.credentials_path):
            self.opts.set(credentials_path=os.path.join(data_dir,
                                                      self.opts.credentials_path))
        self.creds = None
        self.service = None
        self.threads = []
        if self.opts.num_workers > 1:
            self.outq = queue.Queue(maxsize=self.opts.num_workers + 1)
            self.inq = queue.Queue(maxsize=self.opts.num_workers + 1)
            for idx in range(self.opts.num_workers):
                werker = lambda: self.worker(idx, self.outq, self.inq)
                # It's OK for these threads to not free up resources on exit
                # since they don't store permanent state.
                # FIXME: should I even keep a pointer to the tread?
                self.threads.append(threading.Thread(daemon=True,
                                                     target=werker))
                self.threads[idx].start()

    @property
    def poll_interval(self):
        """How often to poll for new messages / updates."""
        return self.opts.poll_interval

    @property
    def scopes(self):
        """Scopes used for authorization."""
        return [scope.rsplit('/', 1)[1] for scope in self.opts.scopes]

    @property
    def writable(self):
        """Whether the account was authorized as read-only or not."""
        return 'gmail.modify' in self.scopes

    @property
    def can_send(self):
        """Whether the scopes list includes the ability to send mail."""
        return ('gmail.compose' in self.scopes or
                'gmail.send' in self.scopes)
    
    def get_credentials(self):
        "Read, or create one if it does not exist, the credentials file."
        store = file.Storage(self.opts.credentials_path)
        creds = store.get()
        if not creds or creds.invalid:
            # Clear out argv so argparse in run_flow() is happy.
            argv = sys.argv
            sys.argv = []
            flow = client.flow_from_clientsecrets(self.opts.client_secret_file,
                                                  self.opts.scopes)
            creds = tools.run_flow(flow, store)
            sys.argv = argv
        return creds

    def reachable(self):
        """Whether the Gmail endpoint is reachable."""
        service = build('gmail', 'v1', http=Http(timeout=1.0))
        url = urlparse.urlparse(service._baseUrl)
        host = url.hostname
        port = url.port
        try:
            socket.getaddrinfo(host, port, proto=socket.IPPROTO_TCP)
        except (socket.herror, socket.gaierror, URLError, OSError):
            return False
        return True

    def authorize(self):
        "Authorize the service to access the user's mailbox."
        if not self.service:
            self.creds = self.get_credentials()
            http = self.creds.authorize(Http(timeout=1.0))
            self.service = build('gmail', 'v1', http=http)
        assert self.service is not None

    def authorized(func):
        "Ensure service is authorized to access the user's mailbox."
        def func_wrap (self, *args, **kwargs):
            if self.service is None:
                self.authorize()
            return func(self, *args, **kwargs)
        return func_wrap

    @authorized
    def get_profile(self):
        "Return the user's profile."
        # Call the Gmail API
        results = self.service.users().getProfile(userId='me').execute()
        return results

    @authorized
    def get_labels(self):
        "Return a list of labels."
        # Call the Gmail API
        results = self.service.users().labels().list(userId='me').execute()
        return results.get('labels', [])

    @authorized
    def get_history_id(self, start=1):
        "Get the current history id of the mailbox."
        try:
            hist = self.service.users().history()
            results = hist.list(userId='me', startHistoryId=start).execute()
            if 'historyId' in results:
                return int(results['historyId'])
            else:
                raise Gmail.GenericException("no historyId field returned")

        except googleapiclient.errors.HttpError:
            # this happens if the original historyId is too old,
            # try to get last message and the historyId from it.
            for mset in self.list_messages(1):
                (_, mset) = mset
                msg = self.get_message(mset[0]['id'])
                return int(msg['historyId'])

    @authorized
    def get_history_since(self, start=0):
        """Get a list of changes since the given start point (a history id)."""
        hist = self.service.users().history()
        try:
            results = hist.list(userId='me', startHistoryId=start).execute()
            if 'history' in results:
                yield results['history']
            while 'nextPageToken' in results:
                results = hist.list(userId='me',
                                    pageToken=results['nextPageToken'],
                                    startHistoryId=start).execute()
                if 'history' in results:
                    yield results['history']

        except googleapiclient.errors.HttpError as ex:
            if ex.resp.status == 404:
                raise Gmail.NoHistoryException
            elif ex.resp.status == 403:
                raise Gmail.UserRateException(ex)
            else:
                raise Gmail.GenericException(ex)

    @authorized
    def list_messages(self, limit=1):
        "Returns a list of messages (max = limit)."
        total = 0
        token = None
        results = []
        while token is None or 'nextPageToken' in results:
            results = self.service.users().messages().list(userId='me',
                                                           pageToken=token,
                                                           q=self.opts.query,
                                                           maxResults=limit,
                                                           includeSpamTrash=True).\
                                                           execute()

            if 'messages' in results:
                total += results['resultSizeEstimate']
                yield results['resultSizeEstimate'], results['messages']
            if 'nextPageToken' in results:
                token = results['nextPageToken']
            if limit is not None and total >= limit:
                break

    @authorized
    def get_message(self, id, format='minimal'):
        """Get the message in the given format."""
        try:
            return self.service.users().messages().get(userId='me',
                                                       id=id,
                                                       format=format).\
                                                       execute()

        except googleapiclient.errors.HttpError as ex:
            if ex.resp.status == 403 or ex.resp.status == 500:
                return self.get_message(id, format)
            else:
                raise ex

    @authorized
    def get_thread(self, id, format='metadata'):
        """Get information abot a thread."""
        try:
            return self.service.users().threads().get(userId='me',
                                                      id=id,
                                                      format=format).\
                                                      execute()

        except googleapiclient.errors.HttpError as ex:
            if ex.resp.status == 403 or ex.resp.status == 500:
                return self.get_thread(id, format)
            else:
                raise ex

    @authorized
    def get_messages(self, ids, format):
        "Get a collection of messages."
        # FIXME: support adaptive batch sizes
        def chunks(l, n):
            "Yield successive n-sized chunks from l."
            for i in range(0, len(l), n):
                yield l[i:i + n]

        if '__getitem__' not in dir(ids):
            ids = (ids, )

        if self.opts.num_workers < 2:
            what = self.service.users().messages()
            for chunk in chunks(ids, self.opts.batch_size):
                try:
                    cmds = [(gid, what.get(userId='me', id=gid,
                                           format=format)) for gid in chunk]
                    responses = Gmail.batch_executor(self.creds, cmds)
                except Gmail.UserRateException as ex:
                    print("remote: user rate error: ", ex)
                except Gmail.BatchException as ex:
                    print("remote: batch request error: ", ex)
                except ConnectionError as ex:
                    print("remote: connection error: ", ex)
                else:
                    yield responses
            return

        idx = 0
        ridx = 0
        pending = {}
        chunks = deque(chunks(ids, self.opts.batch_size))
        what = self.service.users().messages()
        while not (len(chunks) == 0 and idx == ridx):
            if not self.inq.empty():
                try:
                    xx, resp = self.inq.get()
                    pending[xx] = resp
                    while ridx in pending:
                        resp = pending[ridx]
                        del pending[ridx]
                        ridx += 1
                        if isinstance(resp, Exception):
                            raise resp
                        yield resp
                except Gmail.UserRateException as ex:
                    print("remote: user rate error: ", ex)
                except Gmail.BatchException as ex:
                    print("remote: batch request error: ", ex)
                except ConnectionError as ex:
                    print("remote: connection error: ", ex)
                finally:
                    self.inq.task_done()
            if len(chunks) > 0:
                chunk = chunks.popleft()
                cmds = [(gid, what.get(userId='me', id=gid,
                                       format=format)) for gid in chunk]
                self.outq.put([idx, self.creds, cmds])
                idx += 1

        for ridx in sorted(pending.keys()):
            resp = pending[ridx]
            del pending[ridx]
            ridx += 1
            if isinstance(resp, Exception):
                raise resp
            yield resp

    @authorized
    def update_message(self, id, labels):
        """Update a message at the remote endpoint."""
        return(501, 'Not implemented')

    @authorized
    def update_messages(self, ids, add_labels, rm_labels):
        """Update acollection of messages at the remote endpoint."""
        return (501, 'Not implemented')

    @authorized
    def delete_message(self, id):
        """Delete a message at the remote endpoint."""
        return (501, 'Not implemented')

    @authorized
    def search(self, query, labels=[]):
        """Search for messages matching query string.

Query string (query) is further limited to messages with matching labels
(if any).

    :param query str: A query string in Gmail format.

    :param labels list: A list of label names to further limit the
        search.  Only match messages with one or more of the labels in
        the list.

        """
        qstring = query + ' ' + self.opts.query
        if labels:
            query += ' (' + ' OR '.join(['label:' + l for l in labels]) + ')'
        print(query)
        cmd = self.service.users().messages()
        try:
            results = cmd.list(userId='me', q=query,
                               includeSpamTrash=True).execute()
            if 'messages' not in results:
                return []
            gids = [m['id'] for m in results['messages']]
        
            while 'nextPageToken' in results:
                page_token = results['nextPageToken']
                results = cmd.list(userId='me', q=query,
                                   pageToken=page_token,
                                   includeSpamTrash=True).execute()
                gids.extend([m['id'] for m in results['messages']])
            return gids
        except errors.HttpError as ex:
            print('An error occurred: %s' % ex)
        return []
