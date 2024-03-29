import urllib

from flask import jsonify, request
from flask.views import MethodView

from nngmail import get_sync
from nngmail.api import api_bp
from nngmail.models import Account, label_association, Label, Message
from nngmail.api.utils import acct_base, acct_nick_base

## To split a string on whitespace but preserving quoted sub-strings:
##   re.findall(r'[^"\s]\S*|".+?"', line)
##
## Use dateutil.relativetimedelta to add relative dates
##   datetime.now() + rdelta.relativedelta(days-10)

class QueryAPI(MethodView):
    def get(self, account_id):
        #import pdb; pdb.set_trace()
        label_arg = request.args.get('labels', '')
        label_names = urllib.parse.unquote(label_arg).split(',')
        labels = sum(Label.query.with_entities(Label.gid).\
                     filter(Label.name.in_(label_names)).all(), ())
        query = request.args.get('q', None)
        base = Message.query.filter_by(account_id=account_id).\
            join(label_association).join(Label).\
            with_entities(Message.article_id, Label.name).\
            order_by(Message.id.desc())
        if labels:
            base = base.filter(Label.name.in_(labels))

        if query:
            gmail = get_sync(Account.query.get(account_id))
            gids = gmail.search(query, labels)
            # Convert Google ID's to message ID's.
            result = base.filter(Message.google_id.in_(gids)).all()
        else:
            ## If the client doesn't pass a query string, search the
            ## local database for messages matching the criteria.
            result = base.all()
        return jsonify({'result': result})

## Query resource
query_view = QueryAPI.as_view('query')
api_bp.add_url_rule(acct_base + '/querys/', view_func=query_view,
                    methods=['GET'])

@api_bp.route(acct_nick_base + '/querys/', methods=['GET'])
def query_with_nick(nickname):
    return query_view(Account.query.filter_by(nickname=nickname).\
                      first_or_404().id)

