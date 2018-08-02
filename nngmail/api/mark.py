from datetime import datetime
from itertools import groupby
from operator import itemgetter
import time

from flask import jsonify, request
from sqlalchemy import func
from sqlalchemy.sql import and_, or_, not_

from nngmail.api import api_bp
from nngmail.models import Account, Label, Message
from nngmail.api.utils import acct_nick_base

def find_ranges(seq):
    """Convert an ascending list of numbers to a list of ranges.

Each range consists of either a (low, high) tuple, or a single number.

    """
    ranges = []
    for _, g in groupby(enumerate(sorted(seq)), lambda x: x[0] - x[1]):
        group = tuple(map(itemgetter(1), g))
        #group = list(map(int, group))
        if group[0] == group[-1]:
            ranges.append(group[0])
        else:
            ranges.append((group[0], group[-1]))
    return sorted(ranges, key=lambda v: v[0] if isinstance(v, tuple) else v)

@api_bp.route('/labels/<int:label_id>/marks/')
def marks(label_id):
    """Return a list of read, unexit, and unseen messages.

The output of this function is meant for comsumption by Gnus.  Computing
unseen requires the client pass us atimestamp to use as the reference
point (messages received after the timestamp are nuseen).

    """
    stime = time.time()
    label = Label.query.get_or_404(label_id)
    min_aid, max_aid = label.messages.\
        with_entities(func.min(Message.article_id),
                      func.max(Message.article_id)).one()
    start_aid = min(int(request.args.get('fast', '1')), max_aid)

    base_query = label.messages.\
                 with_entities(Message.article_id).\
                 order_by(Message.id.asc())
    if ('timestamp_low' in request.args and
        'timestamp_high' in request.args):
        low = request.args.get('timestamp_low', 0, int)
        high = request.args.get('timestamp_high', 0, int) << 16
        timestamp = datetime.fromtimestamp(low + high)
        query = base_query.filter(or_(Message.article_id > start_aid,
                                      Message.created >= timestamp,
                                      Message.modified >= timestamp))

    else:
        timestamp = None
        query = base_query.filter(Message.article_id > start_aid)

    ## Find all messages that are either:
    ##  - Created after last sync, i.e. new
    ##  - Have an article ID > than what the client knows about
    ##  - Modified since last sync
    all_mids = sum(query.all(), ())

    ## Find unread messages that are marked as unread in the list above.
    unread = sum(label.account.labels.filter_by(name='UNREAD').one().\
                 messages.filter(Message.article_id.in_(all_mids)).\
                 with_entities(Message.article_id).\
                 all(), ())

    ## Find new article ID's.
    new_mids = set(filter(lambda aid: aid > start_aid, all_mids))
    ## Find new unread article ID's.
    new_unread = set(filter(lambda aid: aid > start_aid, unread))

    ## If there are new articles, compute the new unexist set
    if new_mids:
        anum = set(range(start_aid + 1, max_aid + 1))
        new_unexist = anum - new_mids
        new_read = anum - new_unread
    else:
        new_unexist = set()
        new_read = set()

    ## Find the old article ID's that need update
    old_mids = set(filter(lambda aid: aid <= start_aid, all_mids))
    old_unread = set(filter(lambda aid: aid <= start_aid, unread))
    
    ## Find any articles that are not in this label and were
    ## modified since last sync.  They *may* have been in the group
    ## and hence are candidates from insertion into unexist.
    if timestamp:
        removed_query = label.account.messages.\
                        with_entities(Message.article_id).\
                        filter(Message.article_id <= start_aid).\
                        filter(Message.modified >= timestamp).\
                        filter(~Message.labels.any(Label.id == label_id))
        #import pdb; pdb.set_trace()
        add_unexist = set(sum(removed_query.all(), ()))
    else:
        add_unexist = set()

    ## Now compute deltas for read and unexist.  We represent this
    ## as two sets per mark-type; one set is new ID's that should be
    ## in the set, and the other is ID's that should not be in the set.
    rm_unexist = old_mids
    add_read = old_mids - old_unread
    rm_read = old_unread

    if timestamp:
        unseen = set(sum(label.messages.
                         with_entities(Message.article_id).
                         filter(Message.date >= timestamp).all(), ()))
    else:
        unseen = new_unread

    qtime = time.time()

    rtime = time.time()
    marks = {'start-article': start_aid,
             'active': (min_aid, max_aid),
             'new-read': find_ranges(new_read),
             'add-read': find_ranges(add_read),
             'rm-read': find_ranges(rm_read),
             'new-unexist': find_ranges(new_unexist),
             'add-unexist': find_ranges(add_unexist),
             'rm-unexist': find_ranges(rm_unexist),
             'unseen': find_ranges(unseen),
    }
    ftime = time.time()
    serialized = jsonify(marks)
    etime = time.time()
    print('query  time: %7.2f' % (qtime - stime))
    print('set    time: %7.2f' % (rtime - qtime))
    print('range  time: %7.2f' % (ftime - rtime))
    print('serial time: %7.2f' % (etime - ftime))

    return serialized

@api_bp.route(acct_nick_base + '/labels/<string:label>/marks/')
def marks_by_name(nickname, label):
    return marks(Label.query.join(Account).\
                 filter(Account.nickname == nickname).\
                 filter(Label.name == label).first_or_404().id)
