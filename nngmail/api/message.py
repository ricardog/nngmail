import urllib

import click
from flask import abort, jsonify, make_response, render_template, request
from flask.views import MethodView
from sqlalchemy.orm import undefer

from nngmail import db, get_sync, zync
from nngmail.api import api_bp
from nngmail.models import Account, Label, Message
from nngmail.api.utils import acct_base, acct_nick_base

def to_range(r):
    if len(r) == 2:
        return tuple(range(int(r[0]), int(r[1])+1))
    return (int(r[0]), )

def get_ids(arg):
    return sum(map(lambda r: to_range(r),
                   map(lambda s: s.split(':'),
                       arg.split(','))), ())
    
class MessageAPI(MethodView):
    def get(self, account_id, message_id):
        if not message_id:
            ## Return list
            filters = {}
            if 'label' in request.args:
                query = Message.by_label(account_id,
                                         request.args['label']).distinct()
            else:
                query = Message.query.filter_by(account_id=account_id)
            if 'q' in request.args:
                ## FIXME: generalize search query parameters
                q = urllib.parse.unquote(request.args['q'])
                filters.update(dict([q.split('=', 1)]))
                query = query.filter_by(**filters)
            if 'id' in request.args:
                ids = get_ids(request.args['id'])
                query = query.filter(Message.id.in_(ids))
            if 'thread_id' in request.args:
                query = query.filter(thread_id=request.args('thread_id'))
            query = query.order_by(Message.id.desc())
            if 'id' not in request.args:
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
            message = Message.query.get((message_id, account_id))
            if not message:
                return make_response(jsonify({'error': 'Message not found'}),
                                     404)
            fmt = request.args.get('format', 'json')
            if fmt.lower() == 'raw':
                message = Message.query.options(undefer('_raw')).\
                    get((message_id, account_id))
                # FIXME: use stream_with_context?
                if message.raw is None:
                    click.echo('fetching message %d' % message.id)
                    sync = get_sync(message.account).read(message.id)
                    message = Message.query.get((message_id, account_id))
                return make_response(message.raw)
            return jsonify(message)

    def put(self, account_id, message_id):
        def process_message(message, added, removed):
            for label in added:
                message.labels.append(label)
            for label in removed:
                if label not in message.labels:
                    return None
                message.labels.remove(label)
            return message

        if not request.json:
            click.echo('no json')
            return make_response(jsonify({'error':
                                          'No data for message update'}),
                                 400)
        click.echo(request.json)
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
        if account_id and not message_id:
            if 'id' not in request.json:
                abort(404)
            ids = get_ids(request.json['id'])
        else:
            ids = [message_id]
        messages = Message.query.filter_by(account_id=account_id).\
            filter(Message.id.in_(ids)).all()
        resp = sync.remote_batch_update([m.google_id
                                         for m in messages],
                                        [l.gid for l in added],
                                        [l.gid for l in removed])
        if resp:
            click.echo('remote update failed')
            abort(resp[0], resp[1]),
        session = db.session()
        resp = [process_message(m, added, removed) for m in messages]
        session.commit()
        failures = filter(lambda r: r == None, resp)
        return jsonify({'failures': failures})

    def delete(self, account_id, message_id):
        click.echo('delete message %d in account %d' % (message_id, account_id))
        message = Message.query.get((message_id, account_id))
        if not message:
            return make_response(jsonify({'error': 'Message not found'}), 404)
        sync = get_sync(Account.query.get(account_id))
        resp = sync.remote_delete(message.google_id)
        if resp:
            click.echo('remote update failed')
            abort(resp[0], resp[1]),
        db.session().delete(message)
        db.session().commit()
        return jsonify({'result': True})


## Message resource
message_view = MessageAPI.as_view('message')
api_bp.add_url_rule(acct_base + '/messages/', defaults={'message_id': None},
                    view_func=message_view, methods=['GET', 'PUT'])
api_bp.add_url_rule(acct_base + '/messages/<int:message_id>',
                    view_func=message_view, methods=['GET', 'PUT', 'DELETE'])

@api_bp.route(acct_nick_base + '/messages/',
              methods=['GET', 'PUT'])
def messages(nickname):
    account = Account.query.filter_by(nickname=nickname).first_or_404()
    return message_view(account.id, None)

@api_bp.route(acct_nick_base + '/messages/<int:message_id>',
              methods=['GET', 'PUT', 'DELETE'])
def message_by_id(nickname, message_id):
    message = Message.query.get_or_404((message_id, account.id))
    return message_view(message.account_id, message.id)

@api_bp.route(acct_nick_base + '/messages/<string:message_id>',
              methods=['GET', 'PUT', 'DELETE'])
def message_by_message_id(nickname, message_id):
    mid = urllib.parse.unquote(message_id)
    message = Message.query.filter_by(account_id=account.id).\
        filter_by(message_id=mid).first_or_404()
    return message_view(messge.account_id, message.id)
