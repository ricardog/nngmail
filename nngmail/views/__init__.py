from flask import jsonify, request

from nngmail import app
from nngmail.models import Account, Contact, Message
import nngmail.views.account
import nngmail.views.flag
import nngmail.views.message
import nngmail.views.label
import nngmail.views.thread

@app.route('/')
def index():
    return 'Hello World!\n\n'

@app.route('/api/v1.0/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.serialize_list(Contact.query.limit(100).all())})

