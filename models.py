
import enum

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Boolean, Column, DateTime, Enum, Integer
from sqlalchemy import UnicodeText, Unicode, String, Table, ForeignKey
from sqlalchemy.orm import validates, relationship, sessionmaker

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

    @validates('email')
    def validates_email(self, key, email):
        assert '@' in email
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

    __mapper_args__ = {
        'polymorphic_on': type_
      }

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

class Label(UniqueMixin, Base):
    __tablename__ = 'label'
    id = Column(Integer, primary_key=True)
    name = Column(String, unique=True)
    gid = Column(String, unique=True)

    @classmethod
    def unique_hash(cls, gid, name=None):
        return gid

    @classmethod
    def unique_filter(cls, query, gid, name=None):
        return query.filter(Label.gid == gid)

    def __repr__(self):
        return '%s: %s' % (self.name, self.gid)

class Labels(Base):
    __tablename__ = 'label_association'
    id = Column(Integer, primary_key=True)
    label_id = Column(Integer, ForeignKey('label.id'))
    message_id = Column(Integer, ForeignKey('message.id'), index=True)

    label = relationship('Label', backref='labels')
    message = relationship('Message', backref='messages')
    
class Thread(UniqueMixin, Base):
    __tablename__ = 'thread'
    id = Column(Integer, primary_key=True)
    tid = Column('gid', String, unique=True, index=True)

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
    date = Column(DateTime)
    subject = Column(String)
    snippet = Column(String(200))
    deleted = Column(Boolean, default=False)

    from_id = Column(Integer, ForeignKey('contact.id'))
    sender = relationship(Contact, foreign_keys=[from_id], backref='sent')
    thread_id = Column(Integer, ForeignKey('thread.id'), index=True)
    thread = relationship(Thread, foreign_keys=[thread_id], backref='messages')
                          
    to_ = relationship('ToAddressee', cascade='all', backref='received')
    cc = relationship('CcAddressee', cascade='all', backref='cced')
    bcc = relationship('BccAddressee', cascade='all', backref='bcced')
    labels = relationship('Labels', cascade='all', backref='messages')
    
