import time

from flask import abort, jsonify, render_template, request, url_for
from flask.views import MethodView
from sqlalchemy import exc
from sqlalchemy.orm import joinedload

from nngmail import db
from nngmail.api import api_bp
from nngmail.models import Account, Label, Message
from nngmail.api.utils import acct_base, acct_nick_base, get_article_ids

class LabelAPI(MethodView):

    def get(self, account_id, label_id):
        if not label_id:
            ## Return list
            fmt = request.args.get('format', 'json')
            if fmt == 'info':
                data = Label.info(account_id).all()
                info = tuple((dict(zip(['id', 'name', 'gid',
                                        'min', 'max', 'count'], e)))
                             for e in data)
                for obj in info:
                    obj.update({'messages-url':
                                url_for('.label_messages',
                                        label_id=obj['id'],
                                        _external=True),
                                'marks-url': url_for('.marks',
                                                     label_id=obj['id'],
                                                     _external=True)
                    })
                return jsonify({'labels': info})
            query = Label.query.filter_by(account_id=account_id).\
                        order_by(Label.id.desc())
            return jsonify({'labels': query.all()})
        else:
            ## Return single
            label = Label.query.get_or_404(label_id)
            return jsonify(label)

    def delete(self, account_id, label_id):
        label = Label.query.get_or_404(label_id)
        if not label.account.writable:
            abort(403, 'Account is not writable')
        try:
            db.session().delete(label)
            db.session().commit()
        except exc.SQLAlchemyError as ex:
            abort(500, ex)
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
    return view_func(**{'account_id': account_id, 'label_id': label_id})


label_view = LabelAPI.as_view('label')
api_bp.add_url_rule(acct_base + '/labels/', defaults={'label_id': None},
                    view_func=label_view, methods=['GET',])
api_bp.add_url_rule('/labels/<int:label_id>',
                    defaults={'account_id': None},
                    view_func=label_view,
                    methods=['GET', 'DELETE'])

@api_bp.route(acct_nick_base + '/labels/')
def account_labels(nickname):
    return lookup_by_name(nickname, None, label_view)

@api_bp.route(acct_nick_base + '/labels/<string:label>')
def label_by_name(nickname, label):
    return lookup_by_name(nickname, label, label_view)

@api_bp.route(acct_nick_base + '/labels/<string:label>/messages/')
def label_named_messages(nickname, label):
    obj = Label.query.filter(Account.nickname == nickname).\
        filter_by(name=label).first_or_404()
    return label_messages(obj.id, None)

@api_bp.route('/labels/<int:label_id>/messages/',
              defaults={'tag': None})
@api_bp.route('/labels/<int:label_id>/messages/<string:tag>')
def label_messages(label_id, tag):
    label = Label.query.get_or_404(label_id)
    query = label.messages.order_by(Message.id.desc())
    if tag:
        label = Label.query.filter_by(name=tag).\
            filter_by(account_id=label.account_id).first_or_404()
        query = query.filter(Message.labels.any(Label.id == label.id))
    if 'article-id' in request.args:
        ids = get_article_ids(request.args['article-id'])
        query = query.filter(Message.article_id.in_(ids))
    else:
        ## Must appear after the order_by clause
        query = query.limit(request.args.get('limit', 200))
    query = query.options(joinedload(Message.sender))
    stime = time.time()
    messages = query.all()
    qtime = time.time()

    fmt = request.args.get('format', 'json')
    if fmt.lower() == 'nov':
        return render_template('nov.txt', messages=messages)
    if fmt.lower() == 'header':
        return render_template('header.txt', messages=messages)
    ## Skip attributes that require querying the database twice.
    messages = [msg.serialize(omit=['to_', 'cc', 'bcc', 'labels'])
                for msg in messages]
    etime = time.time()
    print('query  time: %7.2f' % (qtime - stime))
    print('serial time: %7.2f' % (etime - qtime))
    return jsonify({'messages': messages})
