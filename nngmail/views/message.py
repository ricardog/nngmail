from flask import jsonify, make_response, request
from flask.views import MethodView

from nngmail import db
from nngmail.models import Account, Message

def account_required(f):
    """Checks whether an account was given or raises error 401."""
    def decorator(*args, **kwargs):
        if 'account_id' not in request.json:
            make_response(jsonify({'error': 'Specify account'}), 401)
        return f(*args, **kwargs)
    return decorator

class MessageAPI(MethodView):
    #decorators = [account_required]
    
    def get(self, account_id, message_id):
        if not message_id:
            ## Return list
            query = Message.query.filter_by(account_id=account_id).\
                limit(200)
            return jsonify({'messages':
                            Message.serialize_list(query.all())})
        else:
            ## Return single
            message = Message.query.get(message_id)
            if not message:
                return make_response(jsonify({'error': 'Not found'}), 404)
            return jsonify(message.serialize())
        
    def delete(self, account_id, message_id):
        message = Message.query.get(message_id)
        if not message:
            return make_response(jsonify({'error': 'Not found'}), 404)
        db.session().delete(message)
        db.session().commit()
        return jsonify({'result': True})
