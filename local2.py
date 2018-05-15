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

    def store(self, gid, thread_id, label_ids, date, size,headers, snippet,
              commit=True):
        keepers = ['From', 'from', 'Subject', 'subject', 'To', 'Cc', 'Bcc']
        headers = dict((hh['name'], hh['value']) for hh in
                       filter(lambda h: h['name'] in keepers, headers))
        if 'From' not in headers and 'from' in headers:
            headers['From'] = headers['from']
        if 'Subject' not in headers and 'subject' in headers:
            headers['Subject'] = headers['subject']
        name, addr = email.utils.parseaddr(headers['From'])
        sender = Contact.as_unique(self.session, name=name, email=addr)
        labels = [self.get_label(gid) for gid in label_ids]
        thread = Thread.as_unique(self.session, tid=thread_id)
        self.session.add(Message(google_id=gid,
                                 thread=thread,
                                 subject=headers.get('Subject', ''),
                                 size=size,
                                 date=datetime.datetime.fromtimestamp(date),
                                 sender=sender,
                                 snippet=snippet,
                                 labels=labels))
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
    
    def find(self, ids):
        if not isinstance(ids, Iterable):
            return self.session.query(Message).filter(Message.id == ids).first()
        return self.session.query(Message).filter(Message.id.in_(ids)).all()

    def find2(self, gids):
        if not isinstance(gids, Iterable):
            return self.session.query(Message).filter(Message.google_id == gids).first()
        return self.session.query(Message).filter(Message.google_id.in_(gids)).all()
    
    def delete(self, gids):
        msgs = self.find(gids)
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
    
