import click
from itertools import groupby
from operator import itemgetter

from flask import abort, jsonify, make_response, render_template, request
from flask.views import MethodView

from nngmail import app, db
from nngmail.models import Account, Label, Message
from nngmail.views.utils import base, acct_base, acct_nick_base
                                                             
class LabelAPI(MethodView):
    def get(self, account_id, label_id):
        if not label_id:
            ## Return list
            fmt = request.args.get('format', 'json')
            if fmt == 'info':
                data = Label.info(account_id).all()
                info = tuple((dict(zip(['id', 'name', 'gid', 'min',
                                        'max', 'count'], e)))
                             for e in data)
                return jsonify({'labels': info})
            query = Label.query.filter_by(account_id=account_id).\
                        order_by(Label.id.desc())
            return jsonify({'labels': query.all()})
        else:
            ## Return single
            label = Label.query.get(label_id)
            if not label:
                return make_response(jsonify({'error': 'Label not found'}),
                                     404)
            return jsonify(label)

    def delete(self, account_id, label_id):
        label = Label.query.get(label_id)
        if not label:
            return make_response(jsonify({'error': 'Label not found'}), 404)
        db.session().delete(label)
        db.session().commit()
        return jsonify({'result': True})

def lookup_by_name(account, label, view_func):
    if isinstance(account, str):
        account_id = Account.query.filter_by(nickname=account).\
            first_or_404().id
    elif isinstance(account, int):
        account_id = account
    elif account is None:
        account_id = None
    else:
        abort(404)

    if isinstance(label, str):
        label_id = Label.query.filter_by(name=label).first_or_404().id
    elif isinstance(label, int):
        label_id = label
    elif label is None:
        label_id = None
    else:
        abort(404)
    return view_func(account_id, label_id)


label_view = LabelAPI.as_view('label_api')
app.add_url_rule(acct_base + '/labels/', defaults={'label_id': None},
                 view_func=label_view, methods=['GET',])
app.add_url_rule(base + '/labels/<int:label_id>',
                 defaults={'account_id': None},
                 view_func=label_view,
                 methods=['GET', 'DELETE'])
    
@app.route(acct_base + '/labels/<string:label>')
def label_by_name(account_id, label):
    return lookup_by_name(account_id, label, label_view)

@app.route(acct_nick_base + '/labels/<string:label>')
def label_by_name2(nickname, label):
    return lookup_by_name(nickname, label, label_view)

@app.route(acct_nick_base + '/labels/')
def label_by_name3(nickname):
    return lookup_by_name(nickname, None, label_view)
