#!/usr/bin/env python

"""
Shows basic usage of the Gmail API.

Lists the user's Gmail labels.
"""
from __future__ import print_function
from apiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools
import queue
import threading

import googleapiclient

import pdb

class Gmail:
    SCOPES = 'https://www.googleapis.com/auth/gmail.readonly'
    CLIENT_SECRET_FILE = 'client_secret.json'
    BATCH_SIZE = 100

    service = None
    credentials_path = None
    query = '-in:chats'

    class GenericException(Exception):
        pass

    class UserRateException(Exception):
        pass

    class BatchException(Exception):
        pass

    class NoHistoryException(Exception):
        pass

    class Worker:
        def __init__(self, idx, inq, outq, creds):
            self.idx = idx
            self.inq = inq
            self.outq = outq
            self.creds = creds
            self.http = None
            self.service = None
            
        def handler(self, rid, resp, ex, messages):
            "Callback invoked by Google API to handled message data."
            def ex_is_error(ex, code):
                "Check if exception is error code 'code'."
                return (type(ex) is googleapiclient.errors.HttpError and
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
            #print('worked %d: received message' % self.idx)
            messages.append(resp)

        def run(self):
            print("worker %d started" % self.idx)

            while True:
                cmd = self.inq.get()
                if cmd is None:
                    break
                if not self.http:
                    self.http = self.creds.authorize(Http())
                if not self.service:
                    self.service = build('gmail', 'v1', http=self.http)

                ridx, gids, format = cmd
                messages = []
                print('worker %d: received request %d' % (self.idx, ridx))
                batch = self.service.new_batch_http_request()
                for gid in gids:
                    batch.add(self.service.users().messages().get(userId='me',
                                                                  id=gid,
                                                                  format=format),
                              callback=lambda a, b, c: self.handler(a, b, c,
                                                                    messages),
                              request_id=gid)
                try:
                    batch.execute()
                except Gmail.UserRateException as ex:
                    print("remote: user rate error, increasing delay")
                    self.outq.put([ridx, ex])
                except Gmail.BatchException as ex:
                    print("reducing batch request size")
                    self.outq.put([ridx, ex])
                except ConnectionError as ex:
                    print("connection failed: ", ex)
                    self.outq.put([ridx, ex])
                except Exception as ex:
                    print("unhandled exception: ", ex)
                    self.outq.put([ridx, ex])
                else:
                    self.outq.put([ridx, messages])
                finally:
                    self.inq.task_done()
                    print('worker %d: completed request %d %d out of %d' %
                          (self.idx, ridx, len(gids), len(messages)))

    def __init__(self):
        "Object for accessing gmail via http API."
        self.credentials_path = 'credentials.json'
        self.creds = None
        self.http = None
        self.service = None
        self.num_workers = 2
        self.workers = []
        self.threads = []
        self.outq = queue.Queue(maxsize=self.num_workers + 1)
        self.inq = queue.Queue(maxsize=self.num_workers + 1)
        self.creds = self.get_credentials()
        for idx in range(self.num_workers):
            self.workers.append(Gmail.Worker(idx, self.outq, self.inq,
                                             self.creds))
            # It's OK for these threads to not free up resources on exit
            # since they don't store permanent state.
            self.threads.append(
                threading.Thread(daemon=True,
                                 target=lambda: self.workers[idx].run()))
            self.threads[idx].start()

    def get_credentials(self):
        "Read, or create one if it does not exist, the credentials file."
        store = file.Storage(self.credentials_path)
        creds = store.get()
        if not creds or creds.invalid:
            flow = client.flow_from_clientsecrets(self.CLIENT_SECRET_FILE,
                                                  self.SCOPES)
            creds = tools.run_flow(flow, store)
        return creds

    def authorize(self):
        "Authorize the service to access the user's mailbox."
        if not self.service:
            self.creds = self.get_credentials()
            self.http = self.creds.authorize(Http())
            self.service = build('gmail', 'v1', http=self.http)
        assert self.service is not None

    def authorized(func):
        "Ensure service is authorized to access the user's mailbox."
        def func_wrap (self, *args, **kwargs):
            if self.service is None:
                self.authorize()
            return func(self, *args, **kwargs)
        return func_wrap

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

        except googleapiclient.errors.HttpError as ex:
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
                yield (results['history'])
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
                raise Gmail.UserRateException(excep)
            else:
                raise Gmail.GenericException(excep)

    @authorized
    def list_messages(self, limit=1):
        "Returns a list of messages (max = limit)."
        total = 0
        pt = None
        while pt is None or 'nextPageToken' in results:
            results = self.service.users().messages().list(userId='me',
                                                           pageToken=pt,
                                                           q=self.query,
                                                           maxResults=limit,
                                                           includeSpamTrash=True).\
                                                           execute()

            if 'messages' in results:
                total += results['resultSizeEstimate']
                yield results['resultSizeEstimate'], results['messages']
            if 'nextPageToken' in results:
                pt = results['nextPageToken']
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
        max_req = self.BATCH_SIZE

        if '__getitem__' not in dir(ids):
            ids = (ids, )

        def chunks(l, n):
            "Yield successive n-sized chunks from l."
            for i in range(0, len(l), n):
                yield l[i:i + n]

        done = False
        idx = 0
        chunker = chunks(ids, self.BATCH_SIZE)
        while True:
            if not self.inq.empty():
                try:
                    ridx, resp = self.inq.get()
                    print('received response %d' % ridx)
                    if isinstance(resp, Exception):
                        pdb.set_trace()
                        raise resp
                    print('response has %d messages' % len(resp))
                    yield resp
                except Gmail.UserRateException as ex:
                    print("remote: user rate error, increasing delay to %s" %
                          user_rate_delay)
                except Gmail.BatchException as ex:
                    print("reducing batch request size to: %d" % max_req)
                except ConnectionError as ex:
                    print("connection failed, re-trying:", ex)
                finally:
                    self.inq.task_done()
            if done and self.inq.empty():
                break
            if not done:
                try:
                    chunk = chunker.__next__()
                except StopIteration:
                    done = True
                    chunk = None
            if chunk:
                print('queuing request %d [%s -> %s]' % (idx, chunk[0],
                                                         chunk[-1]))
                self.outq.put([idx, chunk, format])
                idx += 1

if __name__ == '__main__':
    gmail = Gmail()
    labels = gmail.get_labels()
    if not labels:
        print('No labels found.')
    else:
        print('Labels:')
        for label in labels:
            print(label['name'], ' -- ', label['id'])

    print('Current historyId: %d' % gmail.get_history_id())

    first = True
    for mset in gmail.list_messages(limit=None):
        (total, gids) = mset
        pdb.set_trace()
        print("total %d in %d messages" % (total, len(gids)))
        if first:
            print("Getting messages")
            first = False
            gids = tuple(map(lambda m: m['id'], gids))
            for msg_set in gmail.get_messages(gids, 'metadata'):
                print(len(msg_set))
                for msg in (): #msg_set:
                    print('---')
                    for header in msg['payload']['headers']:
                        print("  %22s: %s" % (header['name'], header['value'][0:80]))
            if None:
                for i, gid in enumerate(gids):
                    data = gmail.get_message(gid, format='full')
                    print("%s" % gid)
                    if 'parts' in data['payload']:
                        print([part['mimeType'] for part in data['payload']['parts']])
                    if i == 0:
                        thread = gmail.get_thread(gid)
                        print(thread.keys())

            msg = gmail.get_message('1548e8329572f23d', 'full')

            import local2
            sql3 = local2.Sqlite3(':memory:')
            sql3.set_history_id(gmail.get_history_id())
            print('Current historyId: %d' % sql3.get_history_id())
            
            gid = msg['id']
            tid = msg['threadId']
            lids = msg['labelIds']
            date = int(msg['internalDate']) / 1000
            headers = msg['payload']['headers']
            snippet = msg['snippet']

            labels = gmail.get_labels()
            if labels:
                for label in labels:
                    sql3.new_label(name=label['name'], gid=label['id'])

            pdb.set_trace()
            sql3.store(gid, tid, lids, date, headers, snippet)
            all_ids = sql3.all_ids()
            pass
        
