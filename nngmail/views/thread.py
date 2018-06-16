import click

from flask import jsonify, make_response, render_template, request
from flask.views import MethodView

from nngmail import app, db
from nngmail.models import Account, Thread
from nngmail.views.utils import base, acct_base, acct_nick_base

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

    def delete(self, account_id, thread_id):
        thread = Thread.query.get(thread_id)
        if not thread:
            return make_response(jsonify({'error': 'Thread not found'}), 404)
        db.session().delete(thread)
        db.session().commit()
        return jsonify({'result': True})

thread_view = ThreadAPI.as_view('thread_api')
app.add_url_rule(acct_base + '/threads/', defaults={'thread_id': None},
                 view_func=thread_view, methods=['GET',])
app.add_url_rule(base + '/threads/<int:thread_id>',
                 defaults={'account_id': None},
                 view_func=thread_view,
                 methods=['GET', 'DELETE'])

@app.route(acct_nick_base + '/threads/<string:tid>',
           methods=['GET', 'DELETE'])
def thread_by_name(nickname, tid):
    account = Account.query.filter_by(nickname=nickname).first_or_404()
    thread = Thread.query.filter_by(account_id=account.id).\
        filter_by(tid=tid).first_or_404()
    return thread_view(None, thread.id)
                       
