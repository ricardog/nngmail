from flask import jsonify, request

from nngmail import app
from nngmail.models import Contact
from nngmail.views.account import AccountAPI
from nngmail.views.message import MessageAPI


@app.route('/')
def index():
    return 'Hello World!'

base = '/api/v1.0'
account_view = AccountAPI.as_view('account_api')
app.add_url_rule(base + '/accounts/', defaults={'account_id': None},
                 view_func=account_view, methods=['GET',])
app.add_url_rule(base + '/accounts/', view_func=account_view,
                 methods=['POST',])
app.add_url_rule(base + '/accounts/<int:account_id>', view_func=account_view,
                 methods=['GET', 'PUT', 'DELETE'])

acct_base = base + '/accounts/<int:account_id>'
message_view = MessageAPI.as_view('message_api')
app.add_url_rule(acct_base + '/messages/', defaults={'message_id': None},
                 view_func=message_view, methods=['GET',])
app.add_url_rule(base + '/messages/<int:message_id>',
                 defaults={'account_id': None},
                 view_func=message_view,
                 methods=['GET', 'DELETE'])

@app.route('/api/v1.0/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.serialize_list(Contact.query.limit(100).all())})

