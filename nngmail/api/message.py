import urllib

import click
from flask import abort, jsonify, make_response, render_template, request
from flask.views import MethodView
from sqlalchemy.orm import undefer

from nngmail import db, get_sync
from nngmail.api import api_bp
from nngmail.models import Account, Label, Message
from nngmail.api.utils import acct_base, acct_nick_base, get_article_ids

class MessageAPI(MethodView):
    def get(self, account_id, message_id):
        if not message_id:
            ## Return list
            query = Message.query.filter_by(account_id=account_id).\
                order_by(Message.id.desc())
            if 'article-id' in request.args:
                ids = get_article_ids(request.args['article-id'])
                query = query.filter(Message.article_id.in_(ids))
            else:
                query = query.limit(request.args.get('limit', 200))
            messages = query.all()

            fmt = request.args.get('format', 'json')
            if fmt.lower() == 'nov':
                return render_template('nov.txt', messages=messages)
            if fmt.lower() == 'header':
                return render_template('header.txt', messages=messages)
            return jsonify({'messages': messages})
        else:
            ## Return single
            message = Message.query.\
                filter(Message.article_id == message_id).\
                first_or_404()
            fmt = request.args.get('format', 'json')
            if fmt.lower() == 'raw':
                message = Message.query.options(undefer('_raw')).\
                    filter(Message.article_id == message_id).\
                    first_or_404()
                # FIXME: use stream_with_context?
                if message.raw is None:
                    click.echo('fetching message %d' % message.id)
                    get_sync(message.account).read(message.id)
                    message = Message.query.get(message_id)
                if message.raw is None:
                    return make_response('\n\nMessage unavailable.\n\n')
                return make_response(message.raw)
            return jsonify(message)

    def put(self, account_id, message_id):
        def process_message(message, added, removed):
            for label in added:
                message.labels.append(label)
            for label in removed:
                if label not in message.labels:
                    return message.id
                message.labels.remove(label)
            return message

        account = Account.query.get_or_404(account_id)
        if not account.writable:
            abort(403, 'Account is not writable')
        if not request.json:
            return make_response(jsonify({'error':
                                          'No data for message update'}),
                                 400)
        if ('add_labels' not in request.json and
            'rm_labels' not in request.json):
            return make_response(jsonify({'error':
                                          'Specify add_labels or rm_labels'}),
                                 400)
        added = [Label.query.filter_by(name=name).first_or_404()
                 for name in request.json.get('add_labels', []) or []]
        removed = [Label.query.filter_by(name=name).first_or_404()
                   for name in request.json.get('rm_labels', []) or []]

        sync = get_sync(Account.query.get(account_id))
        query = Message.query.filter_by(account_id=account_id)
        if account_id and not message_id:
            if 'id' not in request.json:
                abort(404)
            ids = get_article_ids(request.json['article-id'])
            count = len(ids)
            query = query.filter(Message.article_id.in_(ids))
        else:
            count = 1
            query = query.filter(Message.article_id == message_id)
        messages = query.all()
        if len(messages) != count:
            return make_response(jsonify({'error': 'Message not found'}),
                                 404)

        resp = sync.remote_batch_update([m.google_id
                                         for m in messages],
                                        [l.gid for l in added],
                                        [l.gid for l in removed])
        if resp:
            click.echo('remote update failed')
            abort(resp[0], resp[1]),
        session = db.session()
        resp = [process_message(m, added, removed) for m in messages]
        try:
            session.commit()
        except exc.IntegrityError as ex:
            return make_response(jsonify({'error': 'update error'}), 409)
        except exc.SQLAlchemyError as ex:
            abort(500, ex)
        failures = tuple(filter(lambda r: isinstance(r, int), resp))
        return jsonify({'failures': failures})

    def delete(self, account_id, message_id):
        query = Message.query.join(Account).filter(Account.id == account_id)
        if account_id and not message_id:
            if 'article-id' not in request.json:
                abort(404)
            ids = get_article_ids(request.json['article-id'])
            count = len(ids)
            query = query.filter(Message.article_id.in_(ids))
        else:
            query = query.filter(Message.article_id == message_id)
            count = 1
        messages = query.all()

        if len(messages) != count:
            return make_response(jsonify({'error': 'Message not found'}),
                                 404)
        if not messages[0].account.writable:
                abort(403, 'Account is not writable')
        sync = get_sync(Account.query.get(account_id))
        for msg in messages:
            resp = sync.remote_delete(msg.google_id)
            if resp:
                click.echo('remote update failed')
                abort(resp[0], resp[1]),
            db.session().delete(msg)
        try:
            db.session().commit()
        except exc.SQLAlchemyError as ex:
                abort(500, ex)
        return jsonify({'result': True})

## Message resource
message_view = MessageAPI.as_view('message')
#api_bp.add_url_rule(acct_base + '/messages/', defaults={'message_id': None},
#                    view_func=message_view, methods=['GET', 'PUT'])
#api_bp.add_url_rule('/messages/<int:message_id>',
#                    view_func=message_view, methods=['GET', 'PUT', 'DELETE'])

@api_bp.route(acct_nick_base + '/messages/',
              methods=['GET', 'PUT', 'DELETE'])
def messages(nickname):
    account = Account.query.filter_by(nickname=nickname).first_or_404()
    return message_view(account.id, None)

@api_bp.route(acct_nick_base + '/messages/<int:article_id>',
              methods=['GET', 'PUT', 'DELETE'])
def message_by_id(nickname, article_id):
    message = Message.query.join(Account).\
        filter(Account.nickname == nickname).\
        filter(Message.article_id == article_id).\
        first_or_404()
    return message_view(message.account_id, message.id)

@api_bp.route(acct_nick_base + '/messages/<string:message_id>',
              methods=['GET'])
def message_by_message_id(nickname, message_id):
    mid = urllib.parse.unquote(message_id)
    message = Message.query.join(Account).filter(Account.nickname == nickname).\
        filter(Message.message_id == mid).first_or_404()
    return message_view(message.account_id, message.id)
