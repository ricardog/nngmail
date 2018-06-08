import click

from flask import jsonify, make_response, render_template, request
from flask.views import MethodView
from sqlalchemy.orm import undefer

from nngmail import db
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
            if 'limit' in request.args:
                query = Message.query.filter_by(account_id=account_id).\
                    order_by(Message.id.desc()).\
                    limit(request.args.get('limit', 200))
            elif 'id' in request.args:
                ids = sum(map(lambda r: to_range(r),
                              map(lambda s: s.split(':'),
                                  request.args['id'].split(','))), ())
                query = Message.query.filter_by(account_id=account_id).\
                    filter(Message.id.in_(ids)).order_by(Message.id.desc())
            else:
                query = Message.unread(account_id)

            fmt = request.args.get('format', 'json')
            if fmt.lower() == 'nov':
                return render_template('nov.txt', messages=query.all())
            if fmt.lower() == 'header':
                return render_template('header.txt', messages=query.all())
            return jsonify({'messages': query.all()})
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
                click.echo('here you go sir!')
                return make_response(message.raw)
            return jsonify(message)

    def delete(self, account_id, message_id):
        message = Message.query.get(message_id)
        if not message:
            return make_response(jsonify({'error': 'Message not found'}), 404)
        db.session().delete(message)
        db.session().commit()
        return jsonify({'result': True})
