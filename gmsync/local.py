#!/usr/bin/env python

from collections.abc import MutableMapping
from datetime import datetime, timedelta
import email
import logging
import re

from options import Options, OptionsClass

from sqlalchemy.orm import load_only
from sqlalchemy.sql import and_, or_, not_
from sqlalchemy import exc

from nngmail import db
from nngmail.models import Account, KeyValue, Contact
from nngmail.models import Label, Thread, Message
from nngmail.models import ToAddressee, CcAddressee, BccAddressee

import pdb

logger = logging.getLogger('gmsync')

class Sqlite3(OptionsClass):
    """Class for storing message metadata in a local sqlite3 database.

The create functions assume the data is in Gmail format, i.e. the format
Gmail REST API client provides message data.

    """
    options = Options(email=None, nickname=None, account=None,
                      cache_timeout=60, cache_max_size=0,
                      writable=False, can_send=False)

    @staticmethod
    def __new_contacts(session, header):
        """Create new Contracts for each address in a header."""
        contacts = []
        for n, e in email.utils.getaddresses([header]):
            if n == '' and e == '':
                # This hapens with inpropperly quoted emails like
                # dl-engr-silicon@stretchinc.com>
                continue
            try:
                contact = Contact.as_unique(session, email=e, name=n)
                if contact.name == '' and n:
                    contact.name = n
                contacts.append(contact)
            except AssertionError:
                # Contact.as_unique raises an exception of the email
                # field fails validation.  Which happens with improperly
                # quoted headers like (missing quote):
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
                if isinstance(v, MutableMapping):
                    items.extend(flatten(v, new_key, sep=sep).items())
                else:
                    items.append((new_key, v))
            return dict(items)
        self.options = self.options.push(flatten(kwargs))
        self.set(account=Account.as_unique(db.session(),
                                           email=self.options.email,
                                           nickname=self.options.nickname,
                                           writable=self.options.writable,
                                           can_send=False))
        db.session.flush()
        self.label_map = {}
        self.label_imap = {}

    def __build_label_map(self):
        """Construct a has table to map from labels to label object.

This is done to avoid querying the Labels table all the time.

        """
        self.label_map = {}
        self.label_imap = {}
        for label in self.account.labels.all():
            self.label_map[label.gid] = label
            self.label_imap[label.id] = label.gid

    @property
    def account(self):
        """The account we are associated with."""
        return self.options.account

    def get_label(self, name):
        """Get named label object from database."""
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
        session = db.session()
        label = Label.as_unique(session, name=name, gid=gid,
                                account=self.account)
        session.commit()
        self.label_map[label.gid] = label
        self.label_imap[label.id] = label.gid

    def placeholder(self, gids, skip_ok=False):
        """Create placeholder rows for new messages.

Because I use a composite key for the Messages table (id, account_id), I
can't use the standard autoincrement logic for the id column.  Instead I
have to implement it in code.  I tried using the default_value in
SQLAlchemy, but I could not find a way to make things work.

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
        def chunks(l, n):
            "Yield successive n-sized chunks from l."
            for i in range(0, len(l), n):
                yield l[i:i + n]
        
        if not gids:
            return
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        gids = sorted(gids, key=lambda gid: int(gid, 16))
        for chunk in chunks(gids, 10000):
            if skip_ok:
                rows = self.account.messages.\
                            filter(Message.google_id.in_(chunk)).\
                            with_entities(Message.google_id).all()
                exist = sum(map(lambda x: (x['google_id'], ), rows), ())
                chunk = list(set(chunk) - set(exist))
                if not chunk:
                    ## Skip this chunk if all messages already exist
                    continue
            with db.engine.begin() as connection:
                current_id = self.account.messages.\
                    with_entities(db.func.max(Message.article_id)).\
                    first()[0] or 0
                aids = range(current_id + 1, current_id + 1 + len(chunk))
                values = [{'article_id': aid, 'google_id': gid,
                           'account_id': self.account.id, 'message_id': '',
                           'from_id': 1}
                          for aid, gid in zip(aids, chunk)]
                try:
                    connection.execute(Message.__table__.insert().\
                                       values(values))
                except exc.SQLAlchemyError as ex:
                    #pdb.set_trace()
                    import os; os.exit(-1)
                    pass
            
    def process_gmail_message(self, session, mid, msg):
        """Extract message metadata from a Gmail response.

This function "parses" the data we get from Gmail for a message, and
extracts the data we want to store in the DB.

As a side-effect, this function will add to the session a Thread,
Contacts and Addressees that are required to represent the message
metadata.  It does not add label information because the
label_association table does not have a corresponding python class and
so can't be added to the session.

Returns a hash suitable for bulk update using the session (pass as
values).

        """
        ## Look-up the thread (or create it if it does not exist).  The
        ## Unique mixin takes care of adding the new object to the
        ## session.
        thread = Thread.as_unique(session, thread_id=msg['threadId'],
                                  account=self.account)

        keepers = ['From', 'from', 'Subject', 'subject', 'To', 'CC', 'BCC',
                   'Message-ID', 'Message-Id', 'References']
        ## Some messages (drafts?) don't have any headers (nor
        ## labels).
        headers = dict((hh['name'], hh['value']) for hh in
                       filter(lambda h: h['name'] in keepers,
                              msg['payload'].get('headers', {})))

        ## Create all addressee objects and add them to the session.
        ## New contacts get added to the session by the Unique mixin.
        for hdr in ('To', 'CC', 'BCC'):
            cls = (ToAddressee if hdr == 'To' else
                   CcAddressee if hdr == 'CC' else BccAddressee)
            for c in self.__new_contacts(session,
                                           headers.get(hdr, '')):
                session.add(cls(message_id = mid,
                                contact=c))

        ## Create a dict() to hold the message's properties which can be
        ## passed to SA for bulk update (by the caller).
        message_id = headers.get('Message-ID',
                                 headers.get('Message-Id',
                                             '<%s@x.gmail.com>' % msg['id']))
        data = {'thread': thread,
                'message_id': message_id,
                'subject': headers.get('Subject',
                                       headers.get('subject', '')),
                'references': headers.get('References', ''),
                'size': msg.get('sizeEstimate', 0),
                'snippet': msg['snippet'],
        }
        if 'internalDate' in msg:
            data['date'] = datetime.fromtimestamp(int(msg['internalDate']) /
                                                  1000)
        else:
            data['data'] = datetime.now()
        
        senders = self.__new_contacts(session,
                                      headers.get('From',
                                                  headers.get('from', '')))
        if senders:
            data['sender'] = senders[0]

        ## Look-up the label objects--should already be in the DB.
        data['labels'] = [self.get_label(lgid)
                          for lgid in msg.get('labelIds', [])]

        return data
    

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
        msgs = sorted(msgs, key=lambda msg: int(msg['id'], 16))
        gids = [m['id'] for m in msgs]
        session = db.session()
        with session.no_autoflush:
            objs = session.query(Message).\
                filter(and_(Message.google_id.in_(gids),
                            Message.account == self.account)).\
                            order_by(Message.google_id.asc()).all()
            if len(objs) != len(msgs):
                pdb.set_trace()
                pass
            assert len(objs) == len(msgs)

            for obj, msg in zip(objs, msgs):
                assert obj.google_id == msg['id']
                values = self.process_gmail_message(session, obj.id, msg)
                for key, value in values.items():
                    setattr(obj, key, value)
        try:
            session.commit()
        except exc.SQLAlchemyError as ex:
            pdb.set_trace()
            pass


    def update(self, msgs):
        """Update a message in the database.

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
        ohash = dict([(o.google_id, o) for o in objs])
        assert len(objs) == len(msgs)
        for msg in msgs:
            obj = ohash[msg['id']]
            if 'labelIds' in msg:
                obj.labels = [self.get_label(lgid) for lgid in msg['labelIds']]
            else:
                obj.labels = []
            obj.modified = datetime.now()
            logger.info('%s: update: (%d, %d) -> %s' %
                        (self.options.nickname, obj.id, obj.article_id,
                         ', '.join(obj.label_names)))
        session.commit()

    def commit(self):
        """Commit pending session changes to the database."""
        db.session().commit()

    def trash(self, msg):
        """Move a message to the trash."""
        trash = self.get_label('TRASH')
        inbox = self.get_label('INBOX')
        if trash not in msg.labels:
            msg.labels.remove(inbox)
            msg.labels.append(trash)
        msg.updated = datetime.now()

    def untrash(self, msg):
        """Move a message from the trash."""
        trash = self.get_label('TRASH')
        inbox = self.get_label('INBOX')
        if trash in msg.labels:
            msg.labels.remove(trash)
        if inbox not in msg.labels:
            msg.labels.append(trash)
        msg.updated = datetime.now()

    def all_ids(self, since=None):
        """Return all Google id's in the database."""
        query = self.account.messages.options(load_only('google_id'))
        if since:
            if isinstance(since, int):
                since = '%x' % since
            query = query.filter(Message.google_id > since)
        query = query.order_by(Message.google_id.asc())
        return [m.google_id for m in query.all()]

    def find(self, ids, undefer=False):
        """Find messages by id.

When undefer is True, read the raw message body (if available).

        """
        if not ids:
            return
        if '__getitem__' not in dir(ids):
            ids = (ids, )
        query = self.account.messages.filter(Message.id.in_(ids))
        if undefer:
            query = query.undefer('raw')
        return query.all()

    def query_by_gid(self, gids):
        """Return a query to find messages by Google id."""
        if not gids:
            return
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        return self.account.messages.filter(Message.google_id.in_(gids))

    def find_by_gid(self, gids):
        """Find messages by Google id."""
        if not gids:
            return
        return self.query_by_gid(gids).all()

    def gid_to_id(self, gids):
        """Find messages by Google id."""
        if not gids:
            return
        rows = self.query_by_gid(gids).with_entities(Message.id).all()
        return sum(map(lambda x: (x['id'], ), rows), ())

    def find_cacheable(self):
        """Return a list of cacheable message id's."""
        if self.options.cache_timeout == 0:
            return ()
        if self.options.cache_timeout < 0:
            td = datetime.fromtimestamp(0)
        else:
            td = datetime.now() - timedelta(days=self.options.cache_timeout)
        query = self.account.messages.with_entities(Message.id).\
                filter(or_(Message.date > td, Message.modified > td,
                           Message.labels.any(Label.name == 'STARRED')))
        emails = query.all()
        if not emails:
            return ()
        return emails

    def expire_cache(self):
        """Remove message body for messages that have expired."""
        cacheable = self.find_cacheable()
        query = self.account.messages.\
                filter(Message._raw.isnot(None)).\
                filter(~Message.id.in_(cacheable))
        for message in query.all():
            message.raw = None
        db.session.commit()

    def delete(self, gids):
        """Delete a message from the database.

This happens when the message gets deleted in Gmail.

        """
        query = self.account.messages.filter(Message.google_id.in_(gids)).\
            delete(synchronize_session=False)
        db.session.commit()

    def __set_kv(self, key, value, upsert=False):
        """Store a key, value tuple in the database."""
        session = db.session()
        kv = self.account.key_value.filter_by(key=key).first()
        if upsert and kv:
            ## Store the history of history_id for debugging purposes
            kv.value = value
        else:
            kv = KeyValue(account=self.account, key=key, value=value)
        session.add(kv)
        session.commit()

    def __get_kv(self, key):
        """Retrieve a key, value tuple from the database."""
        kv = self.account.key_value.filter_by(key=key).\
            order_by(KeyValue.id.desc()).first()
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

    def set_partial_hid(self, history_id):
        """Set the partial history id for the account."""
        self.__set_kv('partial_id', history_id)

    def get_partial_hid(self):
        """Retrieve the partial history id for the account."""
        value = self.__get_kv('partial_id')
        if value is None:
            return value
        return int(value)

    def set_partial_gid(self, gid):
        """Set the last sync-ed Google ID for the account."""
        self.__set_kv('min_gid', gid, upsert=True)

    def get_partial_gid(self):
        """Retrieve the last sync-ed Google ID for the account."""
        return self.__get_kv('min_gid')

        
