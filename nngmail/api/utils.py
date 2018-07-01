
acct_base = '/accounts/<int:account_id>'
acct_nick_base = '/accounts/<string:nickname>'

def to_range(r):
    """Convert a (low, high) tuple to a sequence of integers."""
    if len(r) == 2:
        return tuple(range(int(r[0]), int(r[1])+1))
    return (int(r[0]), )

def get_ids(arg):
    """Convert a range string to a sequence of integers.

A range string consists of comman separated ranges.  Each range is
either a single number, or a low:high tuple.

    """
    return sum(map(lambda r: to_range(r),
                   map(lambda s: s.split(':'),
                       arg.split(','))), ())
