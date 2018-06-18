
from flask import Blueprint

api_bp = Blueprint('api', __name__, url_prefix='/api/v1.0',
                   template_folder='templates')

import nngmail.api.account
import nngmail.api.flag
import nngmail.api.message
import nngmail.api.label
import nngmail.api.query
import nngmail.api.thread


@api_bp.route('/contacts', methods=['GET'])
def contacts():
    return jsonify({'contacts': Contact.query.limit(100).all()})

