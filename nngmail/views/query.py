from flask import jsonify, request
from flask.views import MethodView

from nngmail import app, db, get_sync
from nngmail.models import Account
from nngmail.views.utils import acct_base, acct_nick_base

class QueryAPI(MethodView):
    def get(self, account_id):
        labels = request.args.get('labels', '').split(',')
        query = request.args.get('q', '')
        gmail = get_sync(Account.query.get(account_id))
        result = gmail.search(query, labels)
        return jsonify({'result': result})

## Query resource
query_view = QueryAPI.as_view('query_api')
app.add_url_rule(acct_base + '/query/', view_func=query_view,
                 methods=['GET'])

@app.route(acct_nick_base + '/query/', methods=['GET'])
def query_with_nick(nickname):
    return query_view(Account.query.filter_by(nickname=nickname).\
                      first_or_404().id)

