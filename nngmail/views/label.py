import click

from flask import jsonify, make_response, render_template, request
from flask.views import MethodView

from nngmail import db
from nngmail.models import Account, Label

class LabelAPI(MethodView):
    def get(self, account_id, label_id):
        if not label_id:
            ## Return list
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
