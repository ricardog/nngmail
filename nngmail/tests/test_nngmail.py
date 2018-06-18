import os
import tempfile

import pytest

from nngmail import app, db


@pytest.fixture
def client():
    app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///:memory:'
    app.config['TESTING'] = True
    client = app.test_client()

    with app.app_context():
        db.create_all()

    yield client

def test_nop(client):
    """Do nothing test."""
    assert True

def test_empty_db(client):
    """Start with a blank database."""

    rv = client.get('/')
    assert b'Hello World!\n\n' == rv.data
