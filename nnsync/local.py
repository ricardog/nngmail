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
    options = Options(email=None, nickname=None, db_url=None, account=None,
                      cache_timeout=60, cache_max_size=0)

    @staticmethod
    def __new_contacts(header):
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
        def flatten(d, parent_key='', sep='_'):
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
                                                nickname=self.opts.nickname))
        self.label_map = {}
        self.label_imap = {}

    def __build_label_map(self):
        self.label_map = {}
        self.label_imap = {}
        for label in Label.query.filter_by(account=self.account).all():
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
        session = db.session()
        label = Label.as_unique(session, name=name, gid=gid,
                                account=self.account)
        session.commit()
        self.label_map[name] = label

    def placeholder(self, gids):
        if not gids:
            return
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        session = db.session()
        values = [{'google_id': gid, 'account_id': self.account.id,}
                  for gid in gids]
        insert = Message.__table__.insert().values(values)
        session.execute(insert)

    def create(self, msgs):
        if '__getitem__' not in dir(msgs):
            msgs = (msgs, )
        session = db.session()
        keepers = ['From', 'from', 'Subject', 'subject', 'To', 'CC', 'BCC',
                   'Message-ID', 'References']
        for msg in msgs:
            if 'headers' not in msg['payload']:
                ## Some message s(drafts?) don't have any headers (nor
                ## labels).
                msg['payload']['headers'] = {}
            headers = dict((hh['name'], hh['value']) for hh in
                           filter(lambda h: h['name'] in keepers,
                                  msg['payload']['headers']))
            default_id = '<%s@mail.gmail.com>' % msg['id']
            senders = self.__new_contacts(headers.get('From',
                                                      headers.get('from', '')))
            if not senders:
                senders.append(None)
            adds = {}
            for hdr in ('To', 'CC', 'BCC'):
                adds[hdr] = self.__new_contacts(headers.get(hdr, ''))
            labels = [self.get_label(lid) for lid in msg.get('labelIds', [])]
            thread = Thread.as_unique(session, tid=msg['threadId'],
                                      account=self.account)
            if 'internalDate' in msg:
                timestamp = datetime.fromtimestamp(int(msg['internalDate']) /
                                                   1000)
            else:
                timestamp = datetime.now()

            session.add(Message(account=self.account, google_id=msg['id'],
                                thread=thread,
                                message_id=headers.get('Message-ID',
                                                       default_id),
                                subject=headers.get('Subject',
                                                    headers.get('subject', '')),
                                references=headers.get('References', ''),
                                size=msg.get('sizeEstimate', 0),
                                date=timestamp, sender=senders[0],
                                snippet=msg['snippet'], labels=labels,
                                tos=adds['To'], ccs=adds['CC'],
                                bccs=adds['BCC']))
        session.commit()

    def update(self, msgs):
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
        db.session().commit()

    def all_ids(self):
        return [m.google_id for m in
                Message.query.options(load_only('google_id')).\
                filter_by(account=self.account).all()]

    def find(self, ids, undefer=False):
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
        if not gids:
            return
        if '__getitem__' not in dir(gids):
            gids = (gids, )
        return Message.query.filter(and_(Message.google_id.in_(gids),
                                         Message.account == self.account)).all()

    def find_cacheable(self):
        td = timedelta(days=self.opts.cache_timeout)
        query = Message.query.with_entities(Message.id).\
                filter(Message.date > datetime.now() - td).\
                filter(Message.account == self.account)
        return sum(query.all(), ())

    def delete(self, gids):
        msgs = self.find_by_gid(gids)
        if msgs:
            session = db.session()
            session.delete(msgs)
            session.commit()

    def __set_kv(self, key, value):
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
        kv = KeyValue.query.filter_by(key=key).\
            filter_by(account=self.account).first()
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
