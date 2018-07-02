#!/usr/bin/env python

import collections
from datetime import datetime, timedelta
import email
import re

from options import Options

from sqlalchemy.orm import load_only
from sqlalchemy.sql import and_, or_, not_

from nngmail import db
from nngmail.models import Account, KeyValue, Contact
from nngmail.models import Label, Thread, Message

import pdb

RE_CATEGORY = re.compile(r'^CATEGORY_([AA-Z]+)$')

class Sqlite3():
    """Class for storing message metadata in a local sqlite3 database.

The create functions assume the data is in Gmail format, i.e. the format
Gmail REST API client provides message data.

    """
    options = Options(email=None, nickname=None, account=None,
                      cache_timeout=60, cache_max_size=0,
                      writable=False, can_send=False)

    @staticmethod
    def __new_contacts(header):
        """Create new Contracts for each address in a header."""
        contacts = []
        for n, e in email.utils.getaddresses([header]):
            if n == '' and e == '':
                # This hapens with inpropperly quoted emails like
                # dl-engr-silicon@stretchinc.com>
                continue
            try:
                contact = Contact.as_unique(db.session, email=e, name=n)
                if contact.name == '' and n:
                    contact.name = n
                contacts.append(contact)
            except AssertionError:
                # Contact.as_unique raises an exception of the email
                # field fails validation.  Which happens with improperly
                # quoted headers like:
                # Wayne Heideman" <wayne@stretchinc.com>
                continue
        return contacts

    def __init__(self, **kwargs):
        """Initialize a new object using the options passed in."""
        def flatten(d, parent_key='', sep='_'):
            """Flatten hierarchical options by adding '_'."""
            items = []
            for k, v in d.items():
                new_key = parent_key + sep + k if parent_key else k
                if isinstance(v, collections.MutableMapping):
                    items.extend(flatten(v, new_key, sep=sep).items())
                else:
                    items.append((new_key, v))
            return dict(items)
        self.opts = self.options.push(flatten(kwargs))
        self.opts.set(account=Account.as_unique(db.session(),
                                                email=self.opts.email,
                                                nickname=self.opts.nickname,
                                                writable=self.opts.writable,
                                                can_send=False))
        self.label_map = {}
        self.label_imap = {}

    def __build_label_map(self):
        """Construct a has table to map from labels to label object.

This is done to avoid querying the Labels table all the time.

        """
        self.label_map = {}
        self.label_imap = {}
        for label in Label.query.filter_by(account=self.account).all():
            self.label_map[label.gid] = label
            self.label_imap[label.id] = label.gid

    @property
    def account(self):
        """The account we are associated with."""
        return self.opts.account

    def get_label(self, name):
        """Get a label object (from database) base on name."""
        if name not in self.label_map:
            self.__build_label_map()
        return self.label_map[name]

    def get_label_gid(self, id):
        if id not in self.label_imap:
            self.__build_label_map()
        return self.label_imap[id]

    def new_label(self, name, gid):
        """Create a new label in the database.

Translate Gmail category labels to something that is easier to read.

        """
        m = RE_CATEGORY.match(name)
        if m:
            name = 'Inbox:%s' % m.group(1).capitalize()
        session = db.session()
        label = Label.as_unique(session, name=name, gid=gid,
                                account=self.account)
        session.commit()
        self.label_map[name] = label

    def placeholder(self, gids):
        """Create placeholder rows for new messages.

Because I use a composite key for the Messages table (id, account_id), I
can't use the standard autoincrement logic for the id column.  Instead I
have to implement it in code.  I tried using the default_value in
SQLAlchemy, but I could nt find a way to make things work.

So instead what I do is start a transaction, read the max id for that
account, and then generate a list of tuples that will create the
appropriate number of rows for the messages we are creating, and commit
the transaction.  I thus end with N mostly empty rows, one per message
to be created.

This trickery slows down import (requires two updates per message
creation) but provides nice article numbers for Gnus.  Without the
composite keys article numbers increase rapidly if you are synchronizing
more than one account.

        """
        if not gids:
            return
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        with db.engine.begin() as connection:
            current_id = Message.query.\
                with_entities(db.func.max(Message.id)).\
                filter_by(account_id=self.account.id).first()[0] or 0
            ids = range(current_id + 1, current_id + 1 + len(gids))
            values = [{'id': id, 'google_id': gid,
                       'account_id': self.account.id, 'message_id': ''}
                      for id, gid in zip(ids, gids)]
            connection.execute(Message.__table__.insert().values(values))

    def create(self, msgs):
        """Create a new message in the database.

This assumes we already created a placeholder row for the message and
focus instead on extracting the required information from the
representation we get from Gmail.

This function will "create" multiple messages at once for efficiency--it
is called once per batch received from Gmail.

        """
        if '__getitem__' not in dir(msgs):
            msgs = (msgs, )
        session = db.session()
        keepers = ['From', 'from', 'Subject', 'subject', 'To', 'CC', 'BCC',
                   'Message-ID', 'Message-Id', 'References']
        objs = Message.query.\
            filter(Message.google_id.in_([msg['id'] for msg in msgs])).all()
        assert len(objs) == len(msgs)
        for obj, msg in zip(sorted(objs, key=lambda o: o.google_id),
                            sorted(msgs, key=lambda m: m['id'])):
            if 'headers' not in msg['payload']:
                ## Some message s(drafts?) don't have any headers (nor
                ## labels).
                msg['payload']['headers'] = {}
            headers = dict((hh['name'], hh['value']) for hh in
                           filter(lambda h: h['name'] in keepers,
                                  msg['payload']['headers']))
            default_id = '<%s@mail.gmail.com>' % msg['id']
            message_id = headers.get('Message-ID',
                                     headers.get('Message-Id', default_id))
            senders = self.__new_contacts(headers.get('From',
                                                      headers.get('from', '')))
            if not senders:
                senders.append(None)
            adds = {}
            for hdr in ('To', 'CC', 'BCC'):
                adds[hdr] = self.__new_contacts(headers.get(hdr, ''))
            labels = [self.get_label(lid) for lid in msg.get('labelIds', [])]
            thread = Thread.as_unique(session, thread_id=msg['threadId'],
                                      account=self.account)
            if 'internalDate' in msg:
                timestamp = datetime.fromtimestamp(int(msg['internalDate']) /
                                                   1000)
            else:
                timestamp = datetime.now()

            obj.thread = thread
            obj.message_id = message_id
            obj.subject = headers.get('Subject',
                                      headers.get('subject', ''))
            obj.references = headers.get('References', '')
            obj.size = msg.get('sizeEstimate', 0)
            obj.date = timestamp
            obj.sender = senders[0]
            obj.snippet = msg['snippet']
            obj.labels = labels
            obj.tos = adds['To']
            obj.ccs = adds['CC']
            obj.bccs = adds['BCC']
            obj.updated = None
        session.commit()

    def update(self, msgs):
        """Update a message in teh database.

Only message labels are allowed to change.  We assume all messages are
already in the database.

This function is rather ineffcient.

        """
        if not msgs:
            return
        if '__getitem__' not in dir(msgs):
            msgs = (msgs, )
        session = db.session()
        objs = self.find_by_gid([m['id'] for m in msgs])
        if len(objs) != len(msgs):
            pdb.set_trace()
        for obj, msg in zip(objs, msgs):
            if 'labelIds' in msg:
                obj.labels = [self.get_label(lgid) for lgid in msg['labelIds']]
            else:
                obj.labels = []
        session.commit()

    def commit(self):
        """Commit pending session changes to the database."""
        db.session().commit()

    def all_ids(self):
        """Return all Google id's in the database."""
        return [m.google_id for m in
                Message.query.options(load_only('google_id')).\
                filter_by(account=self.account).all()]

    def find(self, ids, undefer=False):
        """Find messages by id.

When undefer is True, read the raw message body (if available).

        """
        if not ids:
            return
        if '__getitem__' not in dir(ids):
            ids = (ids, )
        query = Message.query.filter(and_(Message.id.in_(ids),
                                          Message.account == self.account))
        if undefer:
            query = query.undefer('raw')
        return query.all()

    def find_by_gid(self, gids):
        """Find messages by Google id."""
        if not gids:
            return
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        return Message.query.filter(and_(Message.google_id.in_(gids),
                                         Message.account == self.account)).all()

    def find_cacheable(self):
        """Return a list of cacheable message id's."""
        if self.opts.cache_timeout == 0:
            return ()
        if self.opts.cache_timeout < 0:
            td = datetime.fromtimestamp(0)
        else:
            td = datetime.now() - timedelta(days=self.opts.cache_timeout)
        query = Message.query.with_entities(Message.id).\
                filter(or_(Message.date > td,
                           Message.updated > td)).\
                filter(Message.account == self.account)
        return sum(query.all(), ())

    def expire_cache(self):
        """Remove message body for messages that have expired."""
        td = datetime.now() - timedelta(days=self.opts.cache_timeout)
        query = Message.query.\
                filter(Message._raw.isnot(None)).\
                filter(Message.date < td).filter(Message.updated < td).\
                filter(Message.account == self.account)
        for message in query.all():
            self.raw = None
        db.session.commit()

    def delete(self, gids):
        """Delete a message from the database.

This happens when the message gets deleted in Gmail.

        """
        query = Message.query.filter(Message.google_id.in_(gids)).\
            filter_by(account=self.account).\
            delete(synchronize_session=False)
        db.session.commit()

    def __set_kv(self, key, value):
        """Store a key, value tuple in the database."""
        session = db.session()
        kv = KeyValue.query.filter_by(key=key).\
            filter_by(account=self.account).first()
        if kv:
            kv.value = value
        else:
            kv = KeyValue(account=self.account, key=key, value=value)
        session.add(kv)
        session.commit()

    def __get_kv(self, key):
        """Retrieve a key, value tuple from the database."""
        kv = KeyValue.query.filter_by(key=key).\
            filter_by(account=self.account).first()
        if kv:
            return kv.value
        return None

    def set_history_id(self, value):
        """Set the history id for the account."""
        self.__set_kv('history_id', value)

    def get_history_id(self):
        """Retrieve the history id for the account."""
        hid = self.__get_kv('history_id')
        if hid is not None:
            return int(hid)
        return 0
