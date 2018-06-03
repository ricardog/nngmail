import click

from flask import jsonify, make_response, render_template, request
from flask.views import MethodView

from nngmail import db
from nngmail.models import Account, Message

class MessageAPI(MethodView):
    def get(self, account_id, message_id):
        if not message_id:
            ## Return list
            query = Message.query.filter_by(account_id=account_id).\
                order_by(Message.id.desc()).\
                limit(request.args.get('limit', 200))
            fmt = request.args.get('format', 'json')
            if fmt.lower() == 'nov':
                return render_template('nov.txt', messages=query.all())
            if fmt.lower() == 'header':
                return render_template('header.txt', messages=query.all())
            return jsonify({'messages': tuple(query.all())})
        else:
            ## Return single
            message = Message.query.get(message_id)
            if not message:
                return make_response(jsonify({'error': 'Not found'}), 404)
            return jsonify(message)

    def delete(self, account_id, message_id):
        message = Message.query.get(message_id)
        if not message:
            return make_response(jsonify({'error': 'Not found'}), 404)
        db.session().delete(message)
        db.session().commit()
        return jsonify({'result': True})
