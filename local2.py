#!/usr/bin/env python

from collections import Iterable
from datetime import datetime
import email
import re

from options import Options
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, scoped_session
from sqlalchemy.sql import and_, or_, not_

from models import Account, Base, KeyValue, Contact, Label, Thread, Message
import pdb

RE_CATEGORY = re.compile(r'^CATEGORY_([AA-Z]+)$')

class Sqlite3():
    options = Options(email=None, db_url=None, account=None)
    
    @staticmethod
    def __new_contacts(session, header):
        contacts = []
        for n, e in email.utils.getaddresses([header]):
            if n == '' and e== '':
                # This hapens with inpropperly quoted emails like
                # dl-engr-silicon@stretchinc.com>
                continue
            try:
                contacts.append(Contact.as_unique(session, email=e, name=n))
            except AssertionError as ex:
                # Contact.as_unique raises an exception of the email
                # field fails validation.  Which happens with improperly
                # quoted headers like:
                # Wayne Heideman" <wayne@stretchinc.com>
                continue
        return contacts

    def __init__(self, **kwargs):
        global Session
        self.opts = self.options.push(kwargs)
        if '{email}' in self.opts.db_url:
            self.opts.set(db_url=self.opts.db_url.format(email=self.opts.email))
        self.engine = create_engine(self.opts.db_url)
        self.conn = self.engine.connect()
        Base.metadata.create_all(self.engine)
        session_factory = sessionmaker(bind=self.engine)
        Session = scoped_session(session_factory)
        self.session = session_factory()
        session = Session()
        self.opts.set(account=Account.as_unique(session, email=self.opts.email))
        session.commit()
        self.label_map = {}
        self.label_imap = {}

    def __build_label_map(self):
        self.label_map = {}
        self.label_imap = {}
        for label in Session.query(Label).filter_by(account=self.account).all():
            self.label_map[label.gid] = label
            self.label_imap[label.id] = label.gid

    @property
    def account(self):
        return self.opts.account

    def get_label(self, name):
        if name not in self.label_map:
            self.__build_label_map()
        return self.label_map[name]
                               
    def get_label_gid(self, id):
        if id not in self.label_imap:
            self.__build_label_map()
        return self.label_imap[id]

    def new_label(self, name, gid):
        m = RE_CATEGORY.match(name)
        if m:
            name = 'Inbox:%s' % m.group(1).capitalize()
        session = Session()
        label = Label.as_unique(session, name=name, gid=gid,
                                account=self.account)
        session.commit()
        self.label_map[name] = label

    def create(self, msgs):
        if '__getitem__' not in dir(msgs):
            msgs = (msgs, )
        session = Session()
        keepers = ['From', 'from', 'Subject', 'subject', 'To', 'CC', 'BCC',
                   'Message-ID']
        for msg in msgs:
            headers = dict((hh['name'], hh['value']) for hh in
                           filter(lambda h: h['name'] in keepers,
                                  msg['payload']['headers']))
            default_id = '<%s@mail.gmail.com>' % msg['id']
            senders = self.__new_contacts(session,
                                          headers.get('From',
                                                      headers.get('from', '')))
            adds = {}
            for hdr in ('To', 'CC', 'BCC'):
                adds[hdr] = self.__new_contacts(session, headers.get(hdr, ''))
            labels = [self.get_label(lid) for lid in msg.get('labelIds', [])]
            thread = Thread.as_unique(session, tid=msg['threadId'],
                                      account=self.account)
            if 'internalDate' in msg:
                timestamp = datetime.fromtimestamp(int(msg['internalDate']) /
                                                   1000)
            else:
                timestamp = datetime.now()
            session.add(Message(google_id=msg['id'], thread=thread,
                                account=self.account,
                                message_id=headers.get('Message-ID', default_id),
                                subject=headers.get('Subject',
                                                    headers.get('subject', '')),
                                size=msg.get('sizeEstimate', 0),
                                date=timestamp, sender=senders[0],
                                snippet=msg['snippet'],
                                labels=labels, tos=adds['To'], ccs=adds['CC'],
                                bccs=adds['BCC']))
        session.commit()

    def update(self, msgs):
        if '__getitem__' not in dir(msgs):
            msgs = (msgs, )
        session = Session()
        for msg in msgs:
            gid = msg['id']
            labels = Message.find_labels(session, self.account, gid)
            cur = set([l.id for l in labels])
            new = set([self.get_label(lgid).id for lgid in
                       msg.get('labelIds', [])])
            Message.rem_labels(session, gid, list(cur - new))
            Message.add_labels(session, gid, list(new - cur))
        session.commit()

    def new_session(self):
        return Session()

    def commit(self):
        global Session
        Session.commit()

    def all_ids(self):
        global Session
        return [m.google_id for m in
                Session.query(Message).add_column('google_id').\
                filter_by(account=self.account).all()]
    
    def find(self, ids, undefer=False):
        global Session
        if isinstance(ids, str) or not isinstance(ids, Iterable):
            ids = [ids]
        query = Session.query(Message).filter(and_(Message.id.in_(ids),
                                                   Message.account==self.account))
        if undefer:
            query = query.undefer('raw')
        return query.all()

    def find_by_gid(self, gids):
        global Session
        if isinstance(gids, str) or not isinstance(gids, Iterable):
            gids = [gids]
        return Session.query(Message).\
            filter(and_(Message.google_id.in_(gids),
                        Message.account==self.account)).all()
    
    def delete(self, gids):
        global Session
        msgs = self.find_by_gid(gids)
        if msgs:
            session = Session()
            session.delete(msgs)
            session.commit()

    def add_label(self, gid, lid):
        pass

    def rm_label(self, gid, lid):
        pass

    def __set_kv(self, key, value):
        global Session
        session = Session()
        kv = session.query(KeyValue).filter(KeyValue.key == key).first()
        if kv:
            kv.value = value
        else:
            kv = KeyValue(key=key, value=value)
        session.add(kv)
        session.commit()

    def __get_kv(self, key):
        global Session
        kv = Session.query(KeyValue).filter(KeyValue.key == key).first()
        if kv:
            return kv.value
        return None

    def __hid(self):
        return 'history_id_%s' % self.account.email

    def set_history_id(self, value):
        self.__set_kv(self.__hid(), value)

    def get_history_id(self):
        hid = self.__get_kv(self.__hid())
        if hid is not None:
            return int(hid)
        return 0
