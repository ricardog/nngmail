from flask import abort, jsonify, make_response, request, url_for
from flask.views import MethodView
from sqlalchemy import exc

from nngmail import db
from nngmail.api import api_bp
from nngmail.models import Account
from nngmail.api.utils import acct_base, acct_nick_base

## Add a messages-url property to the JSON reprsentation
Account.inject({'messages-url': [url_for, '.messages',
                                 {'nickname': 'nickname',
                                  '_external': True}]
                })

class AccountAPI(MethodView):
    def get(self, account_id):
        if not account_id:
            ## Return list
            return jsonify({'accounts': Account.query.all()})
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
            return make_response(jsonify({'error': 'Bad account email'}), 400)
        account = Account(email=request.json['email'],
                          nickname=request.json['nickname'],
                          writable=request.json.get('writable', False),
                          can_send=request.json.get('can-send', False))
        try:
            db.session().add(account)
            db.session().commit()
        except exc.IntegrityError as ex:
            return make_response(jsonify({'error': 'Acount already exists'}),
                                 409)
        except exc.SQLAlchemyError as ex:
            abort(500, ex)
        return jsonify(account)
        
    def delete(self, account_id):
        account = Account.query.get(account_id)
        if not account:
            return make_response(jsonify({'error': 'Account not found'}), 404)
        try:
            db.session().delete(account)
            db.session().commit()
        except exc.SQLAlchemyError as ex:
            abort(500, ex)
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
        try:
            db.session().commit()
        except exc.IntegrityError as ex:
            return make_response(jsonify({'error': 'update error'}), 409)
        except exc.SQLAlchemyError as ex:
            abort(500, ex)
        return jsonify(account)

## Account resource
account_view = AccountAPI.as_view('account')
api_bp.add_url_rule('/accounts/', defaults={'account_id': None},
                    view_func=account_view, methods=['GET'])
api_bp.add_url_rule('/accounts/', view_func=account_view, methods=['POST'])
api_bp.add_url_rule('/accounts/<int:account_id>', view_func=account_view,
                    methods=['GET', 'PUT', 'DELETE'])
