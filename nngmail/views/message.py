import click

from flask import jsonify, make_response, render_template, request
from flask.views import MethodView
from sqlalchemy.orm import undefer
import urllib

from nngmail import db, get_sync, zync
from nngmail.models import Account, Message

# FIXME: To create a url for a resource use
# click.echo(url_for('message_api', account_id=1))
# click.echo(url_for('message_api', message_id=message.id))

def to_range(r):
    if len(r) == 2:
        return tuple(range(int(r[0]), int(r[1])+1))
    return (int(r[0]), )

class MessageAPI(MethodView):
    def get(self, account_id, message_id):
        if not message_id:
            ## Return list
            filters = {}
            if 'label' in request.args:
                query = Message.by_label(account_id, request.args['label']).distinct()
            else:
                query = Message.query.filter_by(account_id=account_id)
            if 'q' in request.args:
                ## FIXME: generalize search query parameters
                q = urllib.parse.unquote(request.args['q'])
                filters.update(dict([q.split('=', 1)]))
                query = query.filter_by(**filters)
            if 'id' in request.args:
                ids = sum(map(lambda r: to_range(r),
                              map(lambda s: s.split(':'),
                                  request.args['id'].split(','))), ())
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
            message = Message.query.get(message_id)
            if not message:
                return make_response(jsonify({'error': 'Message not found'}),
                                     404)
            fmt = request.args.get('format', 'json')
            if fmt.lower() == 'raw':
                message = Message.query.options(undefer('_raw')).get(message_id)
                # FIXME: use stream_with_context?
                if message.raw is None:
                    click.echo('fetching message %d' % message.id)
                    sync = get_sync(message.account).read(message.id)
                    message = Message.query.get(message_id)
                return make_response(message.raw)
            return jsonify(message)

    def put(self, account_id, message_id):
        if account_id and not message_id:
            account = Account.query.get(account_id)
            if not account:
                return make_response(jsonify({'error':
                                              'Account %d not found' % account_id}),
                                     404)
            if account.nickname not in zync:
                return make_response(jsonify({'error': 'Account %s not syncing' %
                                              account.nickname}), 406)
            if not request.json:
                return make_response(jsonify({'error':
                                              'No data on message put'}),
                                     400)
            if ('cache' in request.json and
                isinstance(request.json['cache'], list)):
                ids = request.json['cache']
                zync[account.nickname][1].put(['read', ids])
            return jsonify({'result': True})
        else:
            return make_response(jsonify({'error': 'Not available'}),
                                 404)         

    def delete(self, account_id, message_id):
        message = Message.query.get(message_id)
        if not message:
            return make_response(jsonify({'error': 'Message not found'}), 404)
        db.session().delete(message)
        db.session().commit()
        return jsonify({'result': True})
