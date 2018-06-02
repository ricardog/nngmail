from flask import jsonify

from nngmail import app
from nngmail.models import Contact
from nngmail.views.account import AccountAPI

@app.route('/')
def index():
    return 'Hello World!'

account_view = AccountAPI.as_view('account_api')
app.add_url_rule('/api/v1.0/accounts/', defaults={'account_id': None},
                 view_func=account_view, methods=['GET',])
app.add_url_rule('/api/v1.0/accounts/', view_func=account_view,
                 methods=['POST',])
app.add_url_rule('/api/v1.0/accounts/<int:account_id>', view_func=account_view,
                 methods=['GET', 'PUT', 'DELETE'])

@app.route('/api/v1.0/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.serialize_list(Contact.query.limit(100).all())})

