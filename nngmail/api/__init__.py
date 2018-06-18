
from flask import Blueprint, jsonify

api_bp = Blueprint('api', __name__, url_prefix='/api/v1.0',
                   template_folder='templates')

import nngmail.api.account
import nngmail.api.flag
import nngmail.api.message
import nngmail.api.label
import nngmail.api.query
import nngmail.api.thread
from nngmail.models import Contact

@api_bp.route('/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.query.limit(100).all()})

