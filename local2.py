#!/usr/bin/env python

import datetime

import enum
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import create_engine
from sqlalchemy import Boolean, Column, DateTime, Enum, Integer
from sqlalchemy import UnicodeText, Unicode, String, Table, ForeignKey
from sqlalchemy.orm import validates, relationship, sessionmaker
from sqlalchemy.sql import select, bindparam, literal, exists

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

class Contact(UniqueMixin, Base):
    __tablename__ = 'contact'
    id = Column(Integer, primary_key=True)
    name = Column('name', String)
    email = Column('email', String, unique=True)

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


association_table = Table('assoc', Base.metadata,
                        Column('contact_id', Integer, ForeignKey('contact.id')),
                        Column('message_id', Integer, ForeignKey('message.id')),
                        Column('type_', Enum(AddresseeEnum)))

class Addressee(Base):
    __tablename__ = 'addressee_association'
    id = Column(Integer, primary_key=True)
    contact_id = Column('contact_id', Integer, ForeignKey('contact.id'))
    message_id = Column('message_id', Integer, ForeignKey('message.id'))
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


class Message(Base):
    __tablename__ = 'message'

    id = Column(Integer, primary_key=True)
    google_id = Column(String)
    subject = Column(String)
    date = Column(DateTime)
    from_id = Column(Integer, ForeignKey('contact.id'))
    sender = relationship(Contact, foreign_keys=[from_id], backref='sent')
    deleted = Column(Boolean, default=False)
    to_ = relationship('ToAddressee', cascade='all', backref='received')
    cc = relationship('CcAddressee', cascade='all', backref='cced')
    bcc = relationship('BccAddressee', cascade='all', backref='bcced')
    

if __name__ == '__main__':
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
    
    session.add(Message(google_id='0xdeadcafe',
                        subject="Let's got for a picnic!",
                        date=datetime.datetime.now(),
                        sender=r,
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
         'data': datetime.datetime.now(),
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
    import pdb; pdb.set_trace()
    pass

def insert_if_not_exists():
    sel = select([literal("1"), literal("John")]).where(
        ~exists([example_table.c.id]).where(example_table.c.id == 1)
    )
    
