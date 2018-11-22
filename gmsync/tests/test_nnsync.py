
import datetime
import os
import pytest
import yaml

import vcr

from nngmail import db
from nngmail.models import Account, Contact, Label, Message
from gmsync import GmSync


TEST_DIR = os.path.dirname(os.path.realpath(__file__))
my_vcr = vcr.VCR(
    #record_mode='new_episode',
    cassette_library_dir=os.path.join(TEST_DIR, 'fixtures'),
)

@pytest.fixture
def sync(client):
    config_file = os.path.join(TEST_DIR, 'test-config.yaml')
    try:
        config = yaml.load(open(config_file, mode='rb'))
    except IOError as ex:
        print('Error: reading config file (%s)' % config_file)
        raise ex
    email = 'ricardog@itinerisinc.com'
    nickname = 'itineris'
    Account.as_unique(db.session(),
                      email=email,
                      nickname=nickname,
                      writable=False,
                      can_send=False)
    db.session().commit()
    
    sync = GmSync(email, nickname, config)
    yield sync

def test_nop(client, sync):
    """Do nothing test."""
    assert True

def test_account(client, sync):
    assert sync.sql3.account.nickname == 'itineris'
    assert sync.sql3.account.email == 'ricardog@itinerisinc.com'
    assert sync.sql3.account.id == 1

@my_vcr.use_cassette(record_mode='new_episodes')
def test_sync_labels(client, sync):
    """List messages in remote server."""
    sync.sync_labels()

    assert Label.query.count() == 21
    label = sync.sql3.get_label('CATEGORY_PROMOTIONS')
    assert label.name == 'CATEGORY_PROMOTIONS'
    assert sync.sql3.get_label_gid(label.id) == 'CATEGORY_PROMOTIONS'
    label = sync.sql3.get_label('CATEGORY_PERSONAL')
    assert label.name == 'CATEGORY_PERSONAL'

    #label = sync.sql3.get_label('CATEGORY_PERSONAL')
    #assert sync.sql3.get_label_gid(label.id) == 'CATEGORY_PERSONAL'
    
@my_vcr.use_cassette()
def test_create(client, sync):
    history_id = sync.create(['124079b56ebb04ba', '124079b58932e67a',
                              '124176af0bfb5565', '1241d0e3048f5ce2',
                              '1243b6dc4e89939b', '1243b72267b97cfb'])
    ids = sum(Message.query.with_entities(Message.id).\
              filter_by(account_id=sync.sql3.account.id).all(), ())
    ## IDs of messages must be sequential
    assert set(ids) == set(range(1, 7))
    assert history_id > 0
    ## Inserted 6 rows into DB
    assert Message.query.count() == 6
    ## Check the contents of first message
    msg = Message.query.filter_by(google_id='124079b56ebb04ba').first()
    assert msg.subject == 'Get started with Gmail'
    assert msg.sender.name == 'Gmail Team'
    assert msg.label_names == ['INBOX']
    assert msg.snippet == 'Gmail is built on the idea that email can be intuitive, efficient, and useful. And maybe even fun.'

@my_vcr.use_cassette()
def test_update(client, sync):
    gids = ['1243b72fcf65ffb3', '12473b759bef320f',
            '124850ec9c21b8e0', '1248ed954ca2070f']
    sync.sql3.placeholder(gids)
    sync.update(gids)
    assert Message.query.count() == 10
    assert Message.query.get(7).label_names == ['INBOX']

@my_vcr.use_cassette()
def test_read(client, sync):
    ids = range(1, 2)
    sync.read(ids)
    msg = Message.query.get(1)
    assert msg.raw is not None
    
def test_update_labels(client, sync):
    new_labels = ['IMPORTANT', 'STARRED', 'CATEGORY_UPDATES', 'INBOX']
    gid = '124079b56ebb04ba'
    updated = {gid: new_labels}
    sync.update_labels(updated)
    msg = Message.query.filter_by(google_id=gid).first()
    assert set([m.gid for m in msg.labels]) == set(new_labels)
    
@my_vcr.use_cassette()
def test_delete(client, sync):
    gids = ['1243b72fcf65ffb3', '12473b759bef320f',
            '124850ec9c21b8e0', '1248ed954ca2070f']
    sync.delete(gids)
    assert Message.query.count() == 6

def test_merge_history(client, sync):
    history = yaml.load(open(os.path.join(TEST_DIR,
                                          'fixtures/history.yaml'), 'rb'))
    merged =  yaml.load(open(os.path.join(TEST_DIR,
                                          'fixtures/merged.yaml'), 'rb'))
    result = sync.merge_history(history)
    assert merged[0] == result[0]
    assert merged[1] == result[1]
    assert merged[2] == result[2]
    assert merged[3] == result[3]

def test_find_cacheable(client, sync):
    with sync.sql3.settings(cache_timeout=0):
        result = sync.sql3.find_cacheable()
        assert result == ()
    with sync.sql3.settings(cache_timeout=-1):
        result = sync.sql3.find_cacheable()
        assert len(result) == Message.query.count()

def test_expire_cache(client, sync):
    with sync.sql3.settings(cache_timeout=0):
        sync.read((1, ))
        sync.expire_cache()
        msg = Message.query.get(1)
        assert msg.raw is None
    with sync.sql3.settings(cache_timeout=-1):
        sync.read((1, ))
        sync.expire_cache()
        msg = Message.query.get(1)
        assert msg.raw is not None


@my_vcr.use_cassette()
def test_incremental_pull1(client, sync):
    # This is what the history ID was for the account on which I
    # recorded the tape.  The value must match what the response
    # contains.
    sync.sql3.set_history_id(291126)
    sync.pull()
    
@my_vcr.use_cassette()
def test_incremental_pull2(client, sync):
    msg = {'from_id': 1,
           'account_id': 1,
           'article_id': 1,
           'google_id': '163e42f92b6526f8',
           'message_id': '<71262888@mail137-235.dal35.mandrillapp.com>',
           'thread_id': 1,
           'from_id': 222,
           'date': datetime.datetime(2018, 6, 9, 11, 55, 11),
           'subject': 'This is a test message',
           'references': '',
           'snippet': 'This is the body of the message',
           'size': 6271}
    sess = db.session()
    mm = Message(**msg)
    sess.add(mm)
    sess.commit()
    # Move history back a few steps so we get some updates.
    sync.sql3.set_history_id(290982)
    sync.pull()
    print("history is %d" % sync.sql3.get_history_id())
    assert sync.sql3.get_history_id() == 291126
