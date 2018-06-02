import click
from flask import jsonify

from nngmail import app, db
from nngmail.models import *

@app.route('/')
def index():
    return 'Hello World!'

@app.route('/api/v1.0/accounts', methods=['GET', 'POST', 'DELETE'])
def accounts():
    return jsonify({'accounts': Account.serialize_list(Account.query.all())})

@app.route('/api/v1.0/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.serialize_list(Contact.query.limit(100).all())})



