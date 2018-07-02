import os
import tempfile

import pytest

def test_nop(client):
    """Do nothing test."""
    assert True

def test_empty_db(client):
    """Start with a blank database."""

    rv = client.get('/')
    assert b'nngmail proxy server is running\n' == rv.data
