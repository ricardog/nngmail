
import enum
import zlib

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import BLOB, Boolean, Column, DateTime, Enum, Integer
from sqlalchemy import UnicodeText, Unicode, String, Table, ForeignKey
from sqlalchemy.ext.associationproxy import association_proxy
from sqlalchemy.orm import deferred, relationship, sessionmaker, validates
from sqlalchemy.orm import synonym
from sqlalchemy.sql import and_, or_, not_

from sqlalchemy.orm import joinedload, Load

Base = declarative_base()

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

class KeyValue(Base):
    __tablename__ = 'kv'
    id = Column(Integer, primary_key=True)
    key = Column(String, unique=True, nullable=False)
    value = Column(String, nullable=False)

class Contact(UniqueMixin, Base):
    __tablename__ = 'contact'
    id = Column(Integer, primary_key=True)
    name = Column(String)
    email = Column(String, unique=True, index=True)

    received = relationship('ToAddressee')
    cced = relationship('CcAddressee')
    bcced = relationship('CcAddressee')

    received_msgs = association_proxy('received', 'message')
    cced_msgs = association_proxy('cced', 'message')
    bcced_msgs = association_proxy('bcced', 'message')
    
    @validates('email')
    def validates_email(self, key, email):
        if '@' not in email:
            print("WARNING: '%s' in email field." % email)
        return email

    @classmethod
    def unique_hash(cls, email, name=None):
        return email

    @classmethod
    def unique_filter(cls, query, email, name=None):
        return query.filter(Contact.email == email)
    
    def __repr__(self):
        return '%s <%s>' % (self.name, self.email)

class Addressee(Base):
    __tablename__ = 'addressee_association'
    id = Column(Integer, primary_key=True)
    contact_id = Column(Integer, ForeignKey('contact.id'))
    message_id = Column(Integer, ForeignKey('message.id'))
    type_ = Column('type_', Enum(AddresseeEnum))

    contact = relationship('Contact')
    message = relationship('Message', backref='addressees')

    name = association_proxy('contact', 'name')
    email = association_proxy('contact', 'email')
    
    def __init__(self, session, type_, email, name):
        self.type_ = type_
        self.contact = Contact.as_unique(session, email=email, name=name)

    __mapper_args__ = {
        'polymorphic_on': type_
      }

class ToAddressee(Addressee):
    def __init__(self, session, email, name):
        Addressee.__init__(self, session, AddresseeEnum.to, email, name)

    __mapper_args__ = {
        'polymorphic_identity': AddresseeEnum.to
    }

class CcAddressee(Addressee):
    def __init__(self, session, email, name):
        Addressee.__init__(self, session, AddresseeEnum.cc, email, name)

    __mapper_args__ = {
        'polymorphic_identity': AddresseeEnum.cc
      }

class BccAddressee(Addressee):
    def __init__(self, session, email, name):
        Addressee.__init__(self, session, AddresseeEnum.bcc, email, name)

    __mapper_args__ = {
        'polymorphic_identity': AddresseeEnum.bcc
      }

label_association = Table('label_association', Base.metadata,
    Column('label_gid', Integer, ForeignKey('label.gid'), nullable=False),
    Column('message_gid', Integer, ForeignKey('message.google_id'),
           index=True, nullable=False)
)

class Label(UniqueMixin, Base):
    __tablename__ = 'label'
    id = Column(Integer, primary_key=True)
    name = Column(String, unique=True)
    gid = Column(String, unique=True)

    messages = relationship('Message', secondary=label_association,
                            lazy='selectin', passive_deletes=True,
                            back_populates='labels')
    
    @classmethod
    def unique_hash(cls, gid, name=None):
        return gid

    @classmethod
    def unique_filter(cls, query, gid, name=None):
        return query.filter(Label.gid == gid)

    def __repr__(self):
        return '%s' % self.name
        
class Thread(UniqueMixin, Base):
    __tablename__ = 'thread'
    id = Column(Integer, primary_key=True)
    tid = Column('gid', String, unique=True, index=True)

    messages = relationship('Message', backref='thread', cascade='all, delete')
    senders = association_proxy('messages', 'sender')
    subjects = association_proxy('messages', 'subject')
    dates = association_proxy('messages', 'date')
    sizes = association_proxy('messages', 'size')
    labels = association_proxy('messages', 'labels')

    @classmethod
    def unique_hash(cls, tid):
        return tid

    @classmethod
    def unique_filter(cls, query, tid):
        return query.filter(Thread.tid == tid)

    def __repr__(self):
        return '%d: %s' % (self.id, self.tid)
    
class Message(Base):
    __tablename__ = 'message'

    id = Column(Integer, primary_key=True)
    google_id = Column(String, index=True, unique=True)
    message_id = Column(String(100), index=True, unique=True, nullable=False)
    date = Column(DateTime)
    subject = Column(String)
    snippet = Column(String(200))
    deleted = Column(Boolean, default=False)
    size = Column(Integer, default=0)
    _raw = deferred(Column(BLOB))

    from_id = Column(Integer, ForeignKey('contact.id'))
    sender = relationship(Contact, foreign_keys=[from_id], backref='sent',
                          innerjoin=True)
    thread_id = Column(Integer, ForeignKey('thread.id'), index=True)

    to_ = relationship('ToAddressee', passive_deletes=True)
    cc = relationship('CcAddressee', passive_deletes=True)
    bcc = relationship('BccAddressee', passive_deletes=True)
    labels = relationship('Label', secondary=label_association,
                          passive_deletes=True,
                          back_populates='messages')
    label_names = association_proxy('labels', 'name')
    tos = association_proxy('to_', 'contact')
    ccs = association_proxy('cc', 'contact')
    bccs = association_proxy('bcc', 'contact')

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
        self.raw2 = raw
        self._raw = zlib.compress(raw)

    raw = synonym("_raw", descriptor=__raw)
        
    @staticmethod
    def find_labels(session, gid):
        # Use non-ORM (i.e. sql) syntax to bypass reading in the Message
        # table itself since updating labels only requires reading the
        # association table.
        return session.query(label_association).filter_by(message_gid=gid).all()

    @staticmethod
    def rem_labels(conn, gid, label_ids):
        if not label_ids:
            return
        q = label_association.delete()
        q = q.where(and_(label_association.c.label_gid.in_(label_ids),
                         label_association.c.message_gid == gid))
        res = conn.execute(q)
        res.close()
        return

    @staticmethod
    def add_labels(conn, gid, label_ids):
        if not label_ids:
            return
        values = [(lid, gid) for lid in label_ids]
        q = label_association.insert()
        res = conn.execute(q.values(values))
        res.close()
        return

def init(fname):
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker, Load, joinedload
    engine = create_engine("sqlite:///%s" % fname)
    conn = engine.connect()
    Base.metadata.create_all(engine)
    sessionmk = sessionmaker(bind=engine)
    session = sessionmk()
    return (engine, session)
