from flask import jsonify, request

from nngmail import app
from nngmail.models import Contact
from nngmail.views.account import AccountAPI
from nngmail.views.message import MessageAPI
from nngmail.views.label import LabelAPI
from nngmail.views.thread import ThreadAPI


@app.route('/')
def index():
    return 'Hello World!\n\n'

base = '/api/v1.0'
acct_base = base + '/accounts/<int:account_id>'

## Account resource
account_view = AccountAPI.as_view('account_api')
app.add_url_rule(base + '/accounts/', defaults={'account_id': None},
                 view_func=account_view, methods=['GET',])
app.add_url_rule(base + '/accounts/', view_func=account_view,
                 methods=['POST',])
app.add_url_rule(base + '/accounts/<int:account_id>', view_func=account_view,
                 methods=['GET', 'PUT', 'DELETE'])

## Message resource
message_view = MessageAPI.as_view('message_api')
app.add_url_rule(acct_base + '/messages/', defaults={'message_id': None},
                 view_func=message_view, methods=['GET', 'PUT'])
app.add_url_rule(base + '/messages/<int:message_id>',
                 defaults={'account_id': None},
                 view_func=message_view,
                 methods=['GET', 'PUT', 'DELETE'])

label_view = LabelAPI.as_view('label_api')
app.add_url_rule(acct_base + '/labels/', defaults={'label_id': None},
                 view_func=label_view, methods=['GET',])
app.add_url_rule(base + '/labels/<int:label_id>',
                 defaults={'account_id': None},
                 view_func=label_view,
                 methods=['GET', 'DELETE'])

thread_view = ThreadAPI.as_view('thread_api')
app.add_url_rule(acct_base + '/threads/', defaults={'thread_id': None},
                 view_func=thread_view, methods=['GET',])
app.add_url_rule(base + '/threads/<int:thread_id>',
                 defaults={'account_id': None},
                 view_func=thread_view,
                 methods=['GET', 'DELETE'])

@app.route('/api/v1.0/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.serialize_list(Contact.query.limit(100).all())})

