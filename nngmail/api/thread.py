from flask import jsonify, make_response, render_template, request
from flask.views import MethodView

from nngmail import db
from nngmail.api import api_bp
from nngmail.models import Account, Thread
from nngmail.api.utils import acct_base, acct_nick_base

class ThreadAPI(MethodView):
    def get(self, account_id, thread_id):
        if not thread_id:
            ## Return list
            query = Thread.query.filter_by(account_id=account_id).\
                        order_by(Thread.id.desc()).\
                        limit(request.args.get('limit', 200))
            return jsonify({'threads': query.all()})
        else:
            ## Return single
            thread = Thread.query.get(thread_id)
            if not thread:
                return make_response(jsonify({'error': 'Thread not found'}),
                                     404)
            fmt = request.args.get('format', 'json')
            if fmt.lower() == 'nov':
                return render_template('nov.txt', messages=thread.messages)
            return jsonify(thread)

    def delete(self, thread_id):
        thread = Thread.query.get(thread_id)
        if not thread:
            return make_response(jsonify({'error': 'Thread not found'}), 404)
        db.session().delete(thread)
        db.session().commit()
        return jsonify({'result': True})

thread_view = ThreadAPI.as_view('thread')
#api_bp.add_url_rule(acct_base + '/threads/', defaults={'thread_id': None},
#                    view_func=thread_view, methods=['GET',])
#api_bp.add_url_rule('/threads/<int:thread_id>',
#                    defaults={'account_id': None},
#                    view_func=thread_view, methods=['GET'])
#api_bp.add_url_rule('/threads/<int:thread_id>', view_func=thread_view,
#                    methods=['DELETE'])

@api_bp.route(acct_nick_base + '/threads/', defaults={'thread_id': None},
              methods=['GET'])
def threads(nickname, thread_id):
    account = Account.query.filter(nickname == nickname).first_or_404()
    return thread_view(account.id, None)

@api_bp.route(acct_nick_base + '/threads/<string:thread_id>',
              methods=['GET', 'DELETE'])
def thread_by_tid(nickname, thread_id):
    thread = Thread.query.filter(Account.nickname == nickname).\
        filter(Thread.thread_id == thread_id).first_or_404()
    return thread_view(None, thread.id)

@api_bp.route(acct_nick_base + '/threads/<int:thread_id>',
              methods=['GET', 'DELETE'])
def thread_by_id(nickname, thread_id):
    thread = Thread.query.filter(Account.nickname == nickname).\
        filter(Thread.id == thread_id).first_or_404()
    return thread_view(None, thread.id)
