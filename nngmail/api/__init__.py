
from flask import Blueprint, jsonify, request

api_bp = Blueprint('api', __name__, url_prefix='/api/v1.0',
                   template_folder='templates')

from nngmail import db, get_sync
import nngmail.api.account
import nngmail.api.mark
import nngmail.api.message
import nngmail.api.label
import nngmail.api.query
import nngmail.api.thread


## Define a route for fetching contacts data
from nngmail.models import Account, Contact

@api_bp.route('/contacts/', methods=['GET'])
def contacts():
    limit = request.args.get('limit', 100)
    return jsonify({'contacts': Contact.query.\
                    order_by(Contact.name.asc()).limit(limit).all()})

@api_bp.route('/sync', methods=['POST'])
def sync():
    for account in Account.query.all():
        if account.nickname != 'no.name':
            get_sync(account).pull()
    return jsonify({'status': 'OK'})
