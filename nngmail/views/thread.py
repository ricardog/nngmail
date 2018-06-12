import click

from flask import jsonify, make_response, render_template, request
from flask.views import MethodView

from nngmail import db
from nngmail.models import Account, Thread

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
