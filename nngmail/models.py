
from datetime import datetime
import enum
import zlib

from sqlalchemy.ext.associationproxy import association_proxy
from sqlalchemy.orm import backref
from sqlalchemy.orm.exc import NoResultFound
from sqlalchemy.inspection import inspect

from nngmail import db
from sqlalchemy.sql import and_, or_, not_

def _unique(session, cls, hashfunc, queryfunc, constructor, arg, kw):
    cache = getattr(session, '_unique_cache', None)
    if cache is None:
        session._unique_cache = cache = {}

    key = (cls, hashfunc(*arg, **kw))
    if key in cache:
        return cache[key]
    else:
        with session.no_autoflush:
            q = session.query(cls)
            q = queryfunc(q, *arg, **kw)
            obj = q.first()
            if not obj:
                obj = constructor(*arg, **kw)
                session.add(obj)
        cache[key] = obj
        return obj

class AddresseeEnum(enum.Enum):
    to = 1
    cc = 2
    bcc = 3

class UniqueMixin(object):
    @classmethod
    def unique_hash(cls, *arg, **kw):
        raise NotImplementedError()

    @classmethod
    def unique_filter(cls, query, *arg, **kw):
        raise NotImplementedError()

    @classmethod
    def as_unique(cls, session, *arg, **kw):
        return _unique(
                    session,
                    cls,
                    cls.unique_hash,
                    cls.unique_filter,
                    cls,
                    arg, kw
               )

class Serializeable(object):
    include = []
    omit = []

    links = {}

    def serialize(self):
        obj = {c: getattr(self, c) for c in
                filter(lambda c: not self.include or c in self.include,
                       filter(lambda c: c not in self.omit,
                              inspect(self).attrs.keys()))}
        if self.links:
            for key, args in  self.links.items():
                if isinstance(args[2], dict):
                    mapped = dict([(k, getattr(self, v)
                                    if v in dir(self) else v)
                                   for k,v in args[2].items()])
                else:
                    mapped = v[2]
                obj.update({key: args[0](args[1], **mapped)})
        return obj

    @classmethod
    def inject(cls, links, omit=[]):
        cls.links = links
        #self.omit.extend(omit)

class TimestampMixin(object):
    created = db.Column(
        db.DateTime, nullable=False, default=datetime.utcnow)
    updated = db.Column(db.DateTime, onupdate=datetime.utcnow)

class KeyValue(db.Model):
    __table_args__ = (db.UniqueConstraint('account_id', 'key',
                                          name='key_1'), )
    id = db.Column(db.Integer, primary_key=True)
    key = db.Column(db.String, nullable=False)
    value = db.Column(db.String, nullable=False)
    account_id = db.Column(db.Integer, db.ForeignKey('account.id'),
                           index=True, nullable=False)

    account = db.relationship('Account',
                              backref=backref('keys', cascade='all,delete'))

class Account(UniqueMixin, TimestampMixin, db.Model, Serializeable):
    id = db.Column(db.Integer, primary_key=True)
    email = db.Column(db.String, unique=True)
    nickname = db.Column(db.String(100), nullable=True)
    writable = db.Column(db.Boolean, default = False)
    can_send = db.Column(db.Boolean, default = False)

    omit = ('messages', 'threads', 'labels', 'keys')

    @db.validates('email')
    def validates_email(self, key, email):
        if '@' not in email:
            print("WARNING: '%s' in email field." % email)
        assert '@' in email
        return email

    @classmethod
    def unique_hash(cls, email, nickname, writable=None, can_send=None):
        return email

    @classmethod
    def unique_filter(cls, query, email, nickname, writable=None,
                      can_send=None):
        return query.filter(Account.email == email)

    def __repr__(self):
        return '%2d: %s <%s>' % (self.id, self.nickname, self.email)

class Contact(UniqueMixin, db.Model, Serializeable):
    __table_args__ = (db.UniqueConstraint('name', 'email',
                                          name='unique_1'), )
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String)
    email = db.Column(db.String, index=True)

    _received = db.relationship('ToAddressee')
    _cced = db.relationship('CcAddressee')
    _bcced = db.relationship('BccAddressee')

    received = association_proxy('_received', 'message')
    cced = association_proxy('_cced', 'message')
    bcced = association_proxy('_bcced', 'message')

    omit = ('sent', '_received', '_cced', '_bcced')

    @db.validates('email')
    def validates_email(self, key, email):
        if '@' not in email:
            print("WARNING: '%s' in email field." % email)
        assert '@' in email
        return email

    @classmethod
    def unique_hash(cls, email, name=None):
        return name + ' ' + email

    @classmethod
    def unique_filter(cls, query, email, name=None):
        return query.filter(Contact.email == email)

    def __repr__(self):
        return '%s <%s>' % (self.name, self.email)

class Addressee(db.Model, Serializeable):
    id = db.Column(db.Integer, primary_key=True)
    contact_id = db.Column(db.Integer, db.ForeignKey('contact.id'),
                           nullable=False)
    message_id = db.Column(db.Integer, db.ForeignKey('message.id'),
                           nullable=False)
    type_ = db.Column('type_', db.Enum(AddresseeEnum))

    contact = db.relationship('Contact')
    message = db.relationship('Message', backref='addressees')

    name = association_proxy('contact', 'name')
    email = association_proxy('contact', 'email')

    __mapper_args__ = {
        'polymorphic_on': type_
      }

    def serialize(self):
        return self.contact.serialize()


class ToAddressee(Addressee):
    __mapper_args__ = {
        'polymorphic_identity': AddresseeEnum.to
    }

class CcAddressee(Addressee):
    __mapper_args__ = {
        'polymorphic_identity': AddresseeEnum.cc
      }

class BccAddressee(Addressee):
    __mapper_args__ = {
        'polymorphic_identity': AddresseeEnum.bcc
      }

label_association = db.Table('label_association',
    db.Column('label_id', db.Integer, db.ForeignKey('label.id'),
              nullable=False),
    db.Column('message_id', db.Integer, db.ForeignKey('message.id'),
              index=True, nullable=False)
)

class Label(UniqueMixin, TimestampMixin, db.Model, Serializeable):
    __table_args__ = (db.UniqueConstraint('account_id', 'gid',
                                          name='gid_1'), )
    id = db.Column(db.Integer, primary_key=True)
    account_id = db.Column(db.Integer, db.ForeignKey('account.id'),
                           nullable=False)
    name = db.Column(db.String)
    gid = db.Column(db.String)

    account = db.relationship('Account',
                              backref=backref('labels', cascade='all,delete'))
    messages = db.relationship('Message', secondary=label_association,
                               lazy='dynamic', passive_deletes=True,
                               back_populates='labels')
    nickname = association_proxy('account', 'nickname')

    omit = ('account', 'messages', 'nickname')

    @classmethod
    def unique_hash(cls, account, gid, name=None):
        return gid

    @classmethod
    def unique_filter(cls, query, account, gid, name=None):
        return query.filter(and_(Label.gid == gid, Label.account == account))

    def __repr__(self):
        return '%s' % self.name

    @staticmethod
    def info(account_id):
        query = Message.query.filter_by(account_id=account_id)
        query = query.join(label_association).join(Label)
        query = query.with_entities(Label.id,
                                    Label.name,
                                    Label.gid,
                                    db.func.min(Message.article_id).label('min_id'),
                                    db.func.max(Message.article_id).label('max_id'),
                                    db.func.count(Message.article_id).label('count'))
        query = query.group_by(label_association.c.label_id)
        return query

class Thread(UniqueMixin, Serializeable, db.Model):
    __table_args__ = (db.UniqueConstraint('account_id', 'thread_id',
                                          name='tid_1'), )
    id = db.Column(db.Integer, primary_key=True)
    account_id = db.Column(db.Integer, db.ForeignKey('account.id'),
                           nullable=False)
    thread_id = db.Column(db.String, index=True)

    account = db.relationship('Account',
                              backref=backref('threads', cascade='all,delete'))
    messages = db.relationship('Message', backref='thread',
                               cascade='all, delete')
    senders = association_proxy('messages', 'sender')
    subjects = association_proxy('messages', 'subject')
    dates = association_proxy('messages', 'date')
    sizes = association_proxy('messages', 'size')
    labels = association_proxy('messages', 'labels')

    omit = ('account', 'messages')

    @classmethod
    def unique_hash(cls, account, thread_id):
        return hash((account, thread_id))

    @classmethod
    def unique_filter(cls, query, account, thread_id):
        return query.filter_by(thread_id=thread_id, account=account)

    def __repr__(self):
        return '%d: %s' % (self.id, self.thread_id)

class Message(TimestampMixin, Serializeable, db.Model):
    __table_args__ = (db.UniqueConstraint('account_id', 'google_id',
                                         name='gid_1'), 
                      db.UniqueConstraint('account_id', 'article_id',
                                          name='aid_1'))

    id = db.Column(db.Integer, primary_key=True, unique=True, nullable=False)
    account_id = db.Column(db.Integer, db.ForeignKey('account.id'),
                           index=True, nullable=False)
    article_id = db.Column(db.Integer, nullable=False)
    google_id = db.Column(db.String(20), index=True, nullable=False)
    message_id = db.Column(db.String(100), index=True, nullable=False)
    thread_id = db.Column(db.Integer, db.ForeignKey('thread.id'), index=True)
    from_id = db.Column(db.Integer, db.ForeignKey('contact.id'), nullable=False)
    date = db.Column(db.DateTime)
    subject = db.Column(db.String)
    references = db.Column(db.String)
    snippet = db.Column(db.String(200))
    size = db.Column(db.Integer, default=0)
    _raw = db.deferred(db.Column(db.BLOB))

    account = db.relationship('Account',
                              backref=backref('messages',
                                              cascade='all,delete'))
    sender = db.relationship(Contact, foreign_keys=[from_id], backref='sent',
                             innerjoin=True)

    to_ = db.relationship('ToAddressee', cascade='all,delete')
    cc = db.relationship('CcAddressee', cascade='all,delete')
    bcc = db.relationship('BccAddressee', cascade='all,delete')
    labels = db.relationship('Label', secondary=label_association,
                             passive_deletes=True,
                             back_populates='messages')
    label_names = association_proxy('labels', 'name')
    tos = association_proxy('to_', 'contact',
                            creator=lambda c: ToAddressee(contact=c))
    ccs = association_proxy('cc', 'contact',
                            creator=lambda c: CcAddressee(contact=c))
    bccs = association_proxy('bcc', 'contact',
                             creator=lambda c: BccAddressee(contact=c))

    omit = ('_raw', 'raw', 'account', 'thread', 'addressees')

    @property
    def __raw(self):
        if self._raw is None:
            return None
        if 'raw2' in dir(self):
            return self.raw2
        self.raw2 = zlib.decompress(self._raw)
        return self.raw2

    @__raw.setter
    def __raw(self, raw):
        if raw is None:
            del self.raw2
            self._raw = None
            return
        self.raw2 = raw
        self._raw = zlib.compress(raw)

    raw = db.synonym("_raw", descriptor=__raw)

    @staticmethod
    def find_labels(session, account, gid):
        # Use non-ORM (i.e. sql) syntax to bypass reading in the Message
        # table itself since updating labels only requires reading the
        # association table.
        return db.session.query(label_association).\
            filter_by(message_gid=gid).\
            join(Label).\
            filter(Label.account==account).all()

    @staticmethod
    def rem_labels(session, gid, label_ids):
        if not label_ids:
            return
        q = label_association.delete()
        q = q.where(and_(label_association.c.label_id.in_(label_ids),
                         label_association.c.message_gid==gid))
        res = session.execute(q)
        res.close()
        return

    @staticmethod
    def add_labels(session, gid, label_ids):
        if not label_ids:
            return
        values = [(lid, gid) for lid in label_ids]
        q = label_association.insert()
        res = session.execute(q.values(values))
        res.close()
        return

    @staticmethod
    def by_label(account_id, label_name='INBOX'):
        query = Message.query.join(label_association).join(Label)
        if account_id:
            query = query.filter(Message.account_id == account_id)
        query = query.filter(Label.name == label_name)
        return query

    @staticmethod
    def unread(account_id=None):
        return Message.by_label(account_id, 'UNREAD')

