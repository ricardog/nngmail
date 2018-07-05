
def test_nop(client):
    """Do nothing test."""
    assert True


def test_empty_db(client):
    """Start with a blank database."""

    rv = client.get('/')
    assert b'nngmail proxy server is running\n' == rv.data


def test_fixture_insert(client, fixtures):
    """Load fixtures into database."""
    assert True


def test_account_list(client):
    """Test account read list."""
    rv = client.get('/api/v1.0/accounts/')
    assert rv.status_code == 200
    assert rv.is_json
    data = rv.get_json()
    assert len(data['accounts']) == 2


def test_account_get(client):
    """Test account read."""
    rv = client.get('/api/v1.0/accounts/1')
    assert rv.status_code == 200
    assert rv.is_json
    data = rv.get_json()
    assert data['nickname'] == 'itineris'

    rv = client.get('/api/v1.0/accounts/3')
    assert rv.status_code == 404


def test_account_post(client):
    """Test account creation."""
    rv = client.post('/api/v1.0/accounts/', json={
        'nickname': 'siarts', 'email': 'ricardog@siliconartisans.com',
        'can_send': False,
        'writable': True
    })
    assert rv.status_code == 200
    assert rv.is_json
    data = rv.get_json()
    assert data['nickname'] == 'siarts'

    rv = client.post('/api/v1.0/accounts/', json={
        'nickname': 'itineris', 'email': 'ricardog@itinerisinc.com',
        'can_send': False,
        'writable': True
    })
    assert rv.status_code == 409


def test_account_put(client):
    """Test account update."""
    rv = client.put('/api/v1.0/accounts/1', json={'nickname': 'new_name',})
    assert rv.status_code == 200
    assert rv.is_json
    data = rv.get_json()
    assert data['nickname'] == 'new_name'

    rv = client.put('/api/v1.0/accounts/1', json={'nickname': 'itineris',})
    assert rv.status_code == 200
    assert rv.is_json
    data = rv.get_json()
    assert data['nickname'] == 'itineris'

    rv = client.put('/api/v1.0/accounts/4', json={'nickname': 'new_name',})
    assert rv.status_code == 404


def test_account_delete(client):
    """Test account removal."""
    rv = client.delete('/api/v1.0/accounts/3')
    assert rv.status_code == 200
    assert rv.is_json

    rv = client.delete('/api/v1.0/accounts/3')
    assert rv.status_code == 404
    

def test_contact_list(client):
    """Test contact read list."""
    rv = client.get('/api/v1.0/contacts/')
    assert rv.status_code == 200
    assert rv.is_json
    contacts = rv.get_json()['contacts']
    assert len(contacts) == 10


def test_contact_other(client):
    """Test other methods not allowed or don't exist."""
    rv = client.get('/api/v1.0/contacts/1')
    assert rv.status_code == 404

    rv = client.post('/api/v1.0/contacts/')
    assert rv.status_code == 405

    rv = client.delete('/api/v1.0/contacts/')
    assert rv.status_code == 405


def test_label_list(client):
    """Test read list of labels."""
    rv = client.get('/api/v1.0/labels/')
    assert rv.status_code == 404

    rv = client.get('/api/v1.0/accounts/1/labels/')
    assert rv.status_code == 200
    assert rv.is_json
    assert len(rv.get_json()['labels']) == 21

    rv = client.get('/api/v1.0/accounts/1/labels/?format=info')
    assert rv.status_code == 200
    assert rv.is_json
    info = rv.get_json()['labels']
    assert len(info) == 3  ## ['Sent Messages', 'SENT', 'INBOX']


def test_label_get(client):
    """Test read label."""
    rv = client.get('/api/v1.0/labels/15')
    assert rv.status_code == 200
    assert rv.is_json
    assert rv.get_json()['name'] == 'INBOX'

def test_label_messages(client):
    """Test read messages with label."""
    rv = client.get('/api/v1.0/labels/15/messages/')
    assert rv.status_code == 200
    assert rv.is_json
    messages = rv.get_json()['messages']
    assert len(messages) == 15
    labels = (tuple(l['name'] for l in m['labels']) for m in messages)
    ## Every message must hve the INBOX label
    assert len(tuple(filter(lambda l: 'INBOX' in l, labels))) == 15

    rv = client.get('/api/v1.0/accounts/itineris/labels/INBOX/messages/')
    assert rv.status_code == 200
    assert rv.is_json
    messages2 = rv.get_json()['messages']
    assert messages == messages2


def test_label_flags(client):
    """Test reading flags for a label."""
    rv = client.get('/api/v1.0/labels/15/flags/')
    assert rv.status_code == 200
    assert rv.is_json
    flags = rv.get_json()
    assert flags['read'] == [[2, 20]]
    assert flags['unexist'] == [4, 13, 15, 16]

    rv = client.get('/api/v1.0/accounts/itineris/labels/INBOX/flags/')
    assert rv.status_code == 200
    assert rv.is_json
    flags2 = rv.get_json()
    assert flags == flags2


def test_label_delete(client):
    """Test deleting a label."""
    rv = client.delete('/api/v1.0/labels/15')
    assert rv.status_code == 200
    assert rv.is_json
    ## FIXME: Deleting a label does not delete messages
    rv = client.get('/api/v1.0/accounts/itineris/messages/')
    assert rv.status_code == 200
    assert rv.is_json
    messages = rv.get_json()['messages']
    assert len(messages) == 20

    rv = client.delete('/api/v1.0/labels/22')
    assert rv.status_code == 403
    assert rv.get_json()['error'] == 'Account is not writable'


def test_message_list(client):
    """Test reading a list of messages."""
    rv = client.get('/api/v1.0/accounts/itineris/messages/')
    assert rv.status_code == 200
    assert rv.is_json
    messages = rv.get_json()['messages']
    assert len(messages) == 20

def test_message_get(client):
    """Test reading a message."""
    rv = client.get('/api/v1.0/accounts/itineris/messages/1')
    assert rv.status_code == 200
    assert rv.is_json
    message = rv.get_json()
    assert message['message_id'] == '<cdcb4b090909291401p35012ea2x@mail.gmail.com>'

    rv = client.get('/api/v1.0/accounts/itineris/messages/' +
                    message['message_id'])
    assert rv.status_code == 200
    assert rv.is_json
    message2 = rv.get_json()
    assert message == message2


def test_messages_put(client):
    """Test updating a collection of messages."""
    rv = client.put('/api/v1.0/accounts/itineris/messages/')
    assert rv.status_code == 400

    rv = client.put('/api/v1.0/accounts/itineris/messages/',
                    json={'bogus': True})
    assert rv.status_code == 400

    rv = client.put('/api/v1.0/accounts/itineris/messages/',
                    json={'add_labels': ['IMPORTANT']})
    assert rv.status_code == 404

    rv = client.put('/api/v1.0/accounts/itineris/messages/200',
                    json={'add_labels': ['IMPORTANT']})
    assert rv.status_code == 404

    rv = client.put('/api/v1.0/accounts/itineris/messages/',
                    json={'id': '1,3,5,7:9',
                          'add_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    rv = client.put('/api/v1.0/accounts/itineris/messages/',
                    json={'id': '1,3,5,7:9',
                          'rm_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    rv = client.put('/api/v1.0/accounts/itineris/messages/',
                    json={'id': '1,3,5,7:9',
                          'rm_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == [1, 3, 5, 7, 8, 9]

    rv = client.put('/api/v1.0/accounts/itineris/messages/',
                    json={'id': '1,3,5,7:9',
                          'add_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    rv = client.put('/api/v1.0/accounts/itineris/messages/',
                    json={'id': '1,3,5,7:9',
                          'add_labels': ['Priority'],
                          'rm_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    rv = client.put('/api/v1.0/accounts/example/messages/')
    assert rv.status_code == 403
    assert rv.get_json()['error'] == 'Account is not writable'


def  test_message_update(client):
    """Test update of a single message."""
    rv = client.put('/api/v1.0/accounts/itineris/messages/200')
    assert rv.status_code == 404

    rv = client.put('/api/v1.0/accounts/itineris/messages/1',
                    json={'bogus': True})
    assert rv.status_code == 400

    rv = client.put('/api/v1.0/accounts/itineris/messages/1',
                    json={'add_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    rv = client.put('/api/v1.0/accounts/itineris/messages/1',
                    json={'rm_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    rv = client.put('/api/v1.0/accounts/itineris/messages/1',
                    json={'rm_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == [1]

    rv = client.put('/api/v1.0/accounts/itineris/messages/1',
                    json={'add_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    rv = client.put('/api/v1.0/accounts/itineris/messages/1',
                    json={'add_labels': ['Priority'],
                          'rm_labels': ['IMPORTANT']})
    assert rv.status_code == 200
    assert rv.get_json()['failures'] == []

    
def test_message_delete(client):
    """Test deleting a message."""
    rv = client.delete('/api/v1.0/accounts/itineris/messages/15')
    assert rv.status_code == 200

    rv = client.delete('/api/v1.0/accounts/itineris/messages/15')
    assert rv.status_code == 404

    rv = client.delete('/api/v1.0/accounts/example/messages/1')
    assert rv.status_code == 403
    assert rv.get_json()['error'] == 'Account is not writable'

    rv = client.delete('/api/v1.0/accounts/itineris/messages/',
                       json={'id': '1'})
    assert rv.status_code == 200
    
