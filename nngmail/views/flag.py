import click
from itertools import groupby
from operator import itemgetter

from flask import abort, jsonify, make_response, render_template, request

from nngmail import app, db
from nngmail.models import Account, Label, Message
from nngmail.views.utils import base, acct_base, acct_nick_base

def find_ranges(seq):
    ranges =[]
    for k,g in groupby(enumerate(seq), lambda x: x[0] - x[1]):
        group = tuple(map(itemgetter(1), g))
        #group = list(map(int, group))
        if group[0] == group[-1]:
            ranges.append(group[0])
        else:
            ranges.append((group[0], group[-1]))
    return sorted(ranges, key=lambda v: v[0] if isinstance(v, tuple) else v)

@app.route(acct_nick_base + '/labels/<string:label>/flags')
def flags_by_name(nickname, label):
    account = Account.query.filter_by(nickname=nickname).first_or_404()
    return flags(Label.query.filter(Label.account == account).first_or_404().id)

@app.route(acct_base + '/labels/<int:label_id>/flags')
def flags(label_id):
    mids = set(sum(Label.query.get_or_404(label_id).messages.\
                   with_entities(Message.id).order_by(Message.id.asc()).\
                   all(), ()))
    unread = set(sum(Label.query.filter_by(name='UNREAD').first().\
                     messages.with_entities(Message.id).\
                     order_by(Message.id.asc()).all(), ()))
    if not mids:
        unseen = ()
        unexist = ()
    else:
        all_mids = set(range(min(mids), max(mids) + 1))
        seen = all_mids - mids
        unseen = mids & unread
        unexist = all_mids - mids
    flags = {'seen': find_ranges(seen), 'unseen': find_ranges(unseen),
             'unexist': find_ranges(unexist)}
    return jsonify({'flags': flags})
