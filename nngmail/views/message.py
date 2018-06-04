import click

from flask import jsonify, make_response, render_template, request
from flask.views import MethodView

from nngmail import db
from nngmail.models import Account, Message

# FIXME: To create a url for a resource use
# click.echo(url_for('message_api', account_id=1))
# click.echo(url_for('message_api', message_id=message.id))

class MessageAPI(MethodView):
    def get(self, account_id, message_id):
        if not message_id:
            ## Return list
            if 'limit' in request.args:
                query = Message.query.filter_by(account_id=account_id).\
                    order_by(Message.id.desc()).\
                    limit(request.args.get('limit', 200))
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
            return jsonify(message)

    def delete(self, account_id, message_id):
        message = Message.query.get(message_id)
        if not message:
            return make_response(jsonify({'error': 'Message not found'}), 404)
        db.session().delete(message)
        db.session().commit()
        return jsonify({'result': True})
