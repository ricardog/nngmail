#!/usr/bin/env python

"""
Shows basic usage of the Gmail API.

Lists the user's Gmail labels.
"""
from __future__ import print_function
import click
from collections import deque
import os
import queue
import sys
import threading
import pdb

from apiclient.discovery import build
import googleapiclient
from httplib2 import Http
from oauth2client import file, client, tools
from options import Options

class Gmail:
    options = Options(email=None,
                      scopes='https://www.googleapis.com/auth/gmail.readonly',
                      client_secret_file='client-secret.json',
                      batch_size=100,
                      credentials_path=None,
                      query='-in:chats',
                      num_workers=0)
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

        http = creds.authorize(Http())
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
        "Object for accessing gmail via http API."
        self.opts = self.options.push(kwargs)
        data_dir = os.path.normpath(os.path.join(os.path.dirname(__file__),
                                                 '../data'))
        print('data dir is %s' % data_dir)
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

    def authorize(self):
        "Authorize the service to access the user's mailbox."
        if not self.service:
            self.creds = self.get_credentials()
            http = self.creds.authorize(Http())
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
