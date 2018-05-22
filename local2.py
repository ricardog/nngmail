#!/usr/bin/env python

from collections import Iterable
from datetime import datetime
import email
import re

from sqlalchemy import create_engine
from models import Base, KeyValue, Contact, Label, Thread, Message
from sqlalchemy.orm import sessionmaker, scoped_session

import pdb

RE_CATEGORY = re.compile(r'^CATEGORY_([AA-Z]+)$')

class Sqlite3():
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

    def __init__(self, fname):
        global Session
        self.engine = create_engine("sqlite:///%s" % fname)
        self.conn = self.engine.connect()
        Base.metadata.create_all(self.engine)
        session_factory = sessionmaker(bind=self.engine)
        Session = scoped_session(session_factory)
        self.session = session_factory()
        self.label_map = {}
        self.label_imap = {}

    def __build_label_map(self):
        self.label_map = {}
        self.label_imap = {}
        for label in Session.query(Label).all():
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
        session = Session()
        label = Label.as_unique(session, name=name, gid=gid)
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
            thread = Thread.as_unique(session, tid=msg['threadId'])
            if 'internalDate' in msg:
                timestamp = datetime.fromtimestamp(int(msg['internalDate']) /
                                                   1000)
            else:
                timestamp = datetime.now()
            session.add(Message(google_id=msg['id'], thread=thread,
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
            labels = Message.find_labels(session, gid)
            cur = set([l.label_gid for l in labels])
            new = set(msg['labelIds'])
            Message.rem_labels(session.connection(), gid, list(cur - new))
            Message.add_labels(session.connection(), gid, list(new - cur))
        session.commit()

    def commit(self):
        Session.commit()

    def all_ids(self):
        return [m.google_id for m in
                Session.query(Message).add_column('google_id').all()]
    
    def find(self, ids, undefer=False):
        if isinstance(ids, str) or not isinstance(ids, Iterable):
            ids = [ids]
        query = Session.query(Message).filter(Message.id.in_(ids))
        if undefer:
            query = query.undefer('raw')
        return query.all()

    def find_by_gid(self, gids):
        if isinstance(gids, str) or not isinstance(gids, Iterable):
            gids = [gids]
        return Session.query(Message).filter(Message.google_id.in_(gids)).all()
    
    def delete(self, gids):
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
        session = Session()
        kv = session.query(KeyValue).filter(KeyValue.key == key).first()
        if kv:
            kv.value = value
        else:
            kv = KeyValue(key=key, value=value)
        session.add(kv)
        session.commit()

    def __get_kv(self, key):
        kv = Session.query(KeyValue).filter(KeyValue.key == key).first()
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
    
if __name__ == '__main__':
    # These imports are only required when running the built-in self-test
    from sqlalchemy.sql import select, bindparam, literal, exists
    from models import ToAddressee, CcAddressee, BccAddressee

    engine = create_engine("sqlite:///:memory:")
    Base.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)
    session = Session()

    r = Contact.as_unique(session, name='Ricardo', email='ricardog@ricardog.com')
    d = Contact.as_unique(session, name='Debby', email='dshepard@gmail.com')
    j = Contact.as_unique(session, name='jax', email='jax@itinerisinc.com')

    r = session.query(Contact).filter(Contact.name == 'Ricardo').first()
    d = session.query(Contact).filter(Contact.name == 'Debby').first()
    j = session.query(Contact).filter(Contact.name == 'Jax').first()

    pers = Label.as_unique(session, gid='personal', name='PERSONAL')
    
    session.add(Message(google_id='0xdeadcafe',
                        subject="Let's got for a picnic!",
                        date=datetime.datetime.now(),
                        sender=r,
                        labels=[Labels(label_id=pers.id)],
                        to_=[ToAddressee(contact_id=r.id)],
                        cc=[CcAddressee(contact_id=d.id)]
    ))
    session.commit()
    
    conn = engine.connect()

    contact = Contact.__table__
    inserter = contact.insert().prefix_with("OR REPLACE")
    result = conn.execute(inserter, [{'email':'ricardog@ricardog.com', 'name': 'Ricardo'},
                            {'email':'dshepard@gmail.com', 'name': 'Debby'},
                            {'email': 'jax@itinerisinc.com', 'name': 'jax'}
    ])


    email_select = select([contact.c.id]).where(
        contact.c.email==bindparam('sender'))

    sender_sel = select([literal("1"), literal("John")]).where(
        ~exists([contact.c.id]).where(contact.c.email == bindparam('sender')))

    message = Message.__table__
    insert = message.insert({'from_id': email_select})

    conn.execute(insert, [
        {'google_id': '0xcafebabe',
         'data': 1525920588,
         'sender': 'ricardog@ricardog.com',
         'subject': "Let's go to the beach!"
        },
        {'google_id': '0xcafebabf',
         'data': datetime.datetime.now(),
         'sender': 'dshepard@gmail.com',
         'subject': "Need to make a budget!"
        },
        {'google_id': '0xcafed0e',
         'data': datetime.datetime.now(),
         'sender': 'jax@itinerisinc.com',
         'subject': "Squirrel!!!!"
        },
    ])

def insert_if_not_exists():
    sel = select([literal("1"), literal("John")]).where(
        ~exists([example_table.c.id]).where(example_table.c.id == 1)
    )
    
