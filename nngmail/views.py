import click
from flask import jsonify, make_response, request
from flask.views import MethodView

from nngmail import app, db
from nngmail.models import *

@app.route('/')
def index():
    return 'Hello World!'

class AccountAPI(MethodView):
    def get(self, account_id):
        if not account_id:
            ## Return list
            return jsonify({'accounts':
                            Account.serialize_list(Account.query.all())})
        else:
            ## Return single
            account = Account.query.get(account_id)
            if not account:
                return make_response(jsonify({'error': 'Not found'}), 404)
            return jsonify(account.serialize())

    def post(self):
        abort(404)

    def delete(self, user_id):
        account = Account.query.get(account_id)
        if not account:
            return make_response(jsonify({'error': 'Not found'}), 404)
        db.session().delete(account)
        db.session().commit()
        return jsonify({'result': True})

    def put(self, account_id):
        account = Account.query.get(account_id)
        if not account:
            return make_response(jsonify({'error': 'Not found'}), 404)
        if not request.json:
            return make_response(jsonify({'error': 'No data'}), 400)
        if ('nickname' not in request.json or
            not isinstance(request.json['nickname'], str)):
            return make_response(jsonify({'error': 'Bad nickname'}), 400)
        account.nickname = request.json['nickname']
        db.session().commit()
        return jsonify(account.serialize())

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



