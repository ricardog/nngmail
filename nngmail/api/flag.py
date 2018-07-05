from datetime import datetime
from itertools import groupby
from operator import itemgetter

from flask import jsonify, request

from nngmail.api import api_bp
from nngmail.models import Account, Label, Message
from nngmail.api.utils import acct_nick_base

def find_ranges(seq):
    """Convert an ascending list of numbers to a list of ranges.

Each range consists of either a (low, high) tuple, or a single number.

    """
    ranges = []
    for _, g in groupby(enumerate(seq), lambda x: x[0] - x[1]):
        group = tuple(map(itemgetter(1), g))
        #group = list(map(int, group))
        if group[0] == group[-1]:
            ranges.append(group[0])
        else:
            ranges.append((group[0], group[-1]))
    return sorted(ranges, key=lambda v: v[0] if isinstance(v, tuple) else v)

@api_bp.route('/labels/<int:label_id>/flags/')
def flags(label_id):
    """Return a list of read, unexit, and unseen messages.

The output of this function is meant for comsumption by Gnus.  Computing
unseen requires the client pass us atimestamp to use as the reference
point (messages received after the timestamp are nuseen).

    """
    label = Label.query.get_or_404(label_id)
    mids = set(sum(label.messages.\
                   with_entities(Message.id).order_by(Message.id.asc()).\
                   all(), ()))
    unread = set(sum(Label.query.filter_by(name='UNREAD').\
                     filter_by(account=label.account).first().\
                     messages.with_entities(Message.id).\
                     order_by(Message.id.asc()).all(), ()))
    if not mids:
        unexist = ()
        read = ()
    else:
        all_mids = set(range(min(mids), max(mids) + 1))
        unexist = all_mids - mids
        read = all_mids - unread
    flags = {'unexist': find_ranges(unexist),
             'read': find_ranges(read)}

    if ('timestamp_low' in request.args and
        'timestamp_high' in request.args):
        low = request.args.get('timestamp_low', 0, int)
        high = request.args.get('timestamp_high', 0, int) << 16
        timestamp = datetime.fromtimestamp(low + high)
        unseen = set(sum(Label.query.get_or_404(label_id).messages.\
                         with_entities(Message.id).\
                         filter(Message.date > timestamp).\
                         order_by(Message.id.asc()).all(), ()))
        flags.update({'unseen': find_ranges(unseen)})

    return jsonify(flags)

@api_bp.route(acct_nick_base + '/labels/<string:label>/flags/')
def flags_by_name(nickname, label):
    return flags(Label.query.filter(Account.nickname == nickname).\
                 filter_by(name=label).first_or_404().id)
