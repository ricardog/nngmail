from flask import jsonify, make_response, request
from flask.views import MethodView

from nngmail import app, db
from nngmail.models import Account
from nngmail.views.utils import base, acct_base, acct_nick_base

class AccountAPI(MethodView):
    def get(self, account_id):
        if not account_id:
            ## Return list
            return jsonify({'accounts': tuple(Account.query.all())})
        else:
            ## Return single
            account = Account.query.get(account_id)
            if not account:
                return make_response(jsonify({'error': 'Account not found'}),
                                     404)
            return jsonify(account)

    def post(self):
        if not request.json:
            return make_response(jsonify({'error': 'No data'}), 400)
        if ('nickname' not in request.json or
            not isinstance(request.json['nickname'], str)):
            return make_response(jsonify({'error': 'Bad account nickname'}),
                                 400)
        if ('email' not in request.json or
            not isinstance(request.json['email'], str)):
            return make_response(jsonify({'error': 'Bad account  email'}), 400)
        account = Account(email=request.json['email'],
                          nickname=request.json['nickname'])
        db.session().add(account)
        db.session().commit()
        return jsonify(account)
        
    def delete(self, account_id):
        account = Account.query.get(account_id)
        if not account:
            return make_response(jsonify({'error': 'Account not found'}), 404)
        db.session().delete(account)
        db.session().commit()
        return jsonify({'result': True})

    def put(self, account_id):
        account = Account.query.get(account_id)
        if not account:
            return make_response(jsonify({'error':
                                          'Account %d not found' % account_id}),
                                 404)
        if not request.json:
            return make_response(jsonify({'error':
                                          'No data on account update'}),
                                 400)
        if ('nickname' not in request.json or
            not isinstance(request.json['nickname'], str)):
            return make_response(jsonify({'error':
                                          'Bad account nickname (%s)' % nickname}),
                                 400)
        account.nickname = request.json['nickname']
        db.session().commit()
        return jsonify(account)

## Account resource
account_view = AccountAPI.as_view('account_api')
app.add_url_rule(base + '/accounts/', defaults={'account_id': None},
                 view_func=account_view, methods=['GET',])
app.add_url_rule(base + '/accounts/', view_func=account_view,
                 methods=['POST',])
app.add_url_rule(base + '/accounts/<int:account_id>', view_func=account_view,
                 methods=['GET', 'PUT', 'DELETE'])
