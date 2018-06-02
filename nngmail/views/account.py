from flask import jsonify, make_response, request
from flask.views import MethodView

from nngmail import db
from nngmail.models import Account

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
        if not request.json:
            return make_response(jsonify({'error': 'No data'}), 400)
        if ('nickname' not in request.json or
            not isinstance(request.json['nickname'], str)):
            return make_response(jsonify({'error': 'Bad nickname'}), 400)
        if ('email' not in request.json or
            not isinstance(request.json['email'], str)):
            return make_response(jsonify({'error': 'Bad email'}), 400)
        account = Account(email=request.json['email'],
                          nickname=request.json['nickname'])
        db.session().add(account)
        db.session().commit()
        return jsonify(account.serialize())
        
    def delete(self, account_id):
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
