#!/usr/bin/env python

import enum
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Date, Integer, String, ForeignKey

Base = declarative_base()

class AddresseeEnum(enum.Enum):
    to = 1
    cc = 2
    bcc = 3


class Address(Base):
    __tablename__ = 'address'

    id = Column(Integer, primary_key=True)
    email = Column(String)
    name = Column(String)

    @validates('email')
    def validates_email(self, key, address):
      assert '@' in address.email
      return address

      
class Addressee(Base):
    __tablename__ = 'addressee_association'
    address_id = Column(Integer, ForeignKey('address.id'))
    message_id = Column(Integer, ForeignKey('message.id'))
    discriminator = Column(String(5))
    __mapper_args__ = {"polymorphic_on": discriminator}


class HasAddressee(object):
    """HasAddressee mixin, creates a relationship to
    the addressee_association table for each parent.

    """
    @declared_attr
    def addressee_association_id(cls):
        return Column(Integer, ForeignKey("addressee_association.id"))

    @declared_attr
    def addressee_association(cls):
        name = cls.__name__
        discriminator = name.lower()

        assoc_cls = type("%sAddresseeAssociation" % name,
                         (AddresseeAssociation, ),
                         dict(__tablename__=None,
                              __mapper_args__={
                                "polymorphic_identity": discriminator
                              }
                         )
        )

        cls.addresses = association_proxy(
                    "addressee_association", "addressee",
                    creator=lambda addressee: assoc_cls(addressee=addressee)
                )
        return relationship(assoc_cls)


class Message(Base):
    __tablename__ = 'message'

    id = Column(Integer, primary_key=True)
    google_id = Column(String)
    subject = Column(String)
    date = Column(DateTime)
    from_id = Column(Integer, ForeignKey('address.id'))
    sender = relationship(Address, foreign_keys=[from_id], backref='messages')
    to_ = relationship('Address', secondary=association_table)
    cc = relationship('Address', secondary=association_table)
    bcc = relationship('Address', secondary=association_table)
    
