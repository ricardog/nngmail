import click
from flask import jsonify, make_response, request

from nngmail import app, db
from nngmail.models import *

@app.route('/')
def index():
    return 'Hello World!'

@app.route('/api/v1.0/accounts', methods=['GET'])
def get_accounts():
    return jsonify({'accounts': Account.serialize_list(Account.query.all())})

@app.route('/api/v1.0/accounts/<int:account_id>', methods=['GET'])
def get_account(account_id):
    account = Account.query.get(account_id)
    if not account:
        return make_response(jsonify({'error': 'Not found'}), 404)
    return jsonify(account.serialize())

@app.route('/api/v1.0/accounts/<int:account_id>', methods=['PUT'])
def update_account(account_id):
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

@app.route('/api/v1.0/accounts/<int:account_id>', methods=['DELETE'])
def delete_account(account_id):
    account = Account.query.get(account_id)
    if not account:
        return make_response(jsonify({'error': 'Not found'}), 404)
    db.session().delete(account)
    db.session().commit()
    return jsonify({'result': True})

@app.route('/api/v1.0/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.serialize_list(Contact.query.limit(100).all())})



