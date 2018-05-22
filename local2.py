#!/usr/bin/env python

from collections import Iterable
import datetime
import email
import re

from sqlalchemy import create_engine
from models import Base, KeyValue, Contact, Label, Thread, Message
from sqlalchemy.orm import sessionmaker

import pdb

RE_CATEGORY = re.compile(r'^CATEGORY_([AA-Z]+)$')


class Sqlite3():
    def __new_contacts(self, header):
        contacts = []
        for n, e in email.utils.getaddresses([header]):
            if n == '' and e== '':
                # This hapens with inpropperly quoted emails like
                # dl-engr-silicon@stretchinc.com>
                continue
            try:
                contacts.append(Contact.as_unique(self.session, email=e,
                                                  name=n))
            except AssertionError as ex:
                # Contact.as_unique raises an exception of the email
                # field fails validation.  Which happens with improperly
                # quoted headers like:
                # Wayne Heideman" <wayne@stretchinc.com>
                continue
        return contacts

    def __init__(self, fname):
        self.engine = create_engine("sqlite:///%s" % fname)
        self.conn = self.engine.connect()
        Base.metadata.create_all(self.engine)
        sessionmk = sessionmaker(bind=self.engine)
        self.session = sessionmk()
        self.label_map = {}
        self.label_imap = {}
        
    def __build_label_map(self):
        self.label_map = {}
        self.label_imap = {}
        for label in self.session.query(Label).all():
            self.label_map[label.gid] = label
            self.label_imap[label.id] = label.gid

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
        label = Label.as_unique(self.session, name=name, gid=gid)
        self.label_map[name] = label

    def create(self, google_id, thread_id, commit=True, **kwargs):
        keepers = ['From', 'from', 'Subject', 'subject', 'To', 'CC', 'BCC',
                   'Message-ID']
        headers = dict((hh['name'], hh['value']) for hh in
                       filter(lambda h: h['name'] in keepers,
                              kwargs.get('headers', [])))
        default_id = '<%s@mail.gmail.com>' % google_id
        senders = self.__new_contacts(headers.get('From',
                                                  headers.get('from', '')))
        adds = {}
        for hdr in ('To', 'CC', 'BCC'):
            adds[hdr] = self.__new_contacts(headers.get(hdr, ''))
        labels = [self.get_label(lid) for lid in kwargs.get('label_ids', [])]
        thread = Thread.as_unique(self.session, tid=thread_id)
        if 'date' in kwargs:
            timestamp = datetime.datetime.fromtimestamp(kwargs.get('date'))
        else:
            timestamp = datetime.satetime.now()
        self.session.add(Message(google_id=google_id,
                                 thread=thread,
                                 message_id=headers.get('Message-ID',
                                                        default_id),
                                 subject=headers.get('Subject',
                                                     headers.get('subject',
                                                                 '')),
                                 size=kwargs.get('size', 0),
                                 date=timestamp,
                                 sender=senders[0],
                                 snippet=kwargs.get('snippet', ''),
                                 labels=labels,
                                 tos=adds['To'], ccs=adds['CC'],
                                 bccs=adds['BCC']))
        if commit:
            self.session.commit()

    def commit(self):
        self.session.commit()

    def update(self, gid, label_ids):
        labels = Message.find_labels(self.session, gid)
        cur = set([l.label_gid for l in labels])
        new = set(label_ids)
        Message.rem_labels(self.session.connection(), gid, list(cur - new))
        Message.add_labels(self.session.connection(), gid, list(new - cur))

    def all_ids(self):
        return [m.google_id for m in
                self.session.query(Message).add_column('google_id').all()]
    
    def find(self, ids, undefer=False):
        if isinstance(ids, str) or not isinstance(ids, Iterable):
            ids = [ids]
        query = self.session.query(Message).filter(Message.id.in_(ids))
        if undefer:
            query = query.undefer('raw')
        return query.all()

    def find_by_gid(self, gids):
        if isinstance(gids, str) or not isinstance(gids, Iterable):
            gids = [gids]
        return self.session.query(Message).filter(Message.google_id.in_(gids)).all()
    
    def delete(self, gids):
        msgs = self.find_by_gid(gids)
        if msgs:
            self.session.delete(msgs)
            self.session.commit()

    def add_label(self, gid, lid):
        pass

    def rm_label(self, gid, lid):
        pass

    def __set_kv(self, key, value):
        kv = self.session.query(KeyValue).filter(KeyValue.key == key).first()
        if kv:
            kv.value = value
        else:
            kv = KeyValue(key=key, value=value)
        self.session.add(kv)
        self.session.commit()

    def __get_kv(self, key):
        kv = self.session.query(KeyValue).filter(KeyValue.key == key).first()
        if kv:
            return kv.value
        return None

    def set_history_id(self, value):
        self.__set_kv('history_id', value)

    def get_history_id(self):
        hid = self.__get_kv('history_id')
        if hid is not None:
            return int(hid)
        return 0
