from flask import jsonify, request
from flask.views import MethodView
import urllib

from nngmail import app, db, get_sync
from nngmail.models import Account, label_association, Label, Message
from nngmail.views.utils import acct_base, acct_nick_base

class QueryAPI(MethodView):
    def get(self, account_id):
        label_arg = request.args.get('labels', '')
        label_names = urllib.parse.unquote(label_arg).split(',')
        labels = sum(Label.query.with_entities(Label.gid).\
                     filter(Label.name.in_(label_names)).all(), ())
        query = request.args.get('q', '')
        gmail = get_sync(Account.query.get(account_id))
        gids = gmail.search(query, labels)
        # Convert Google ID's to message ID's.
        result = Message.query.filter_by(account_id=account_id).\
            join(label_association).join(Label).\
            with_entities(Message.id, Label.name).\
            filter(Message.google_id.in_(gids)).\
            filter(Label.name.in_(labels)).order_by(Message.id.desc()).all()
        return jsonify({'result': result})

## Query resource
query_view = QueryAPI.as_view('query_api')
app.add_url_rule(acct_base + '/querys/', view_func=query_view,
                 methods=['GET'])

@app.route(acct_nick_base + '/querys/', methods=['GET'])
def query_with_nick(nickname):
    return query_view(Account.query.filter_by(nickname=nickname).\
                      first_or_404().id)

