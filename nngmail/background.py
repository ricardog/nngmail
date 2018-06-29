import atexit
import os
import yaml

from nngmail import app, db, zync
from nngmail.models import Account
from nnsync import NnSync

def load_config():
    """Load the nnsync config file."""
    config_file = os.path.normpath(os.path.join(app.root_path, '..',
                                                'data', 'config.yaml'))
    try:
        return yaml.load(open(config_file, mode='rb'))
    except IOError:
        print('Error: reading config file (%s)' % config_file)
        return {}

@app.before_first_request
def background():
    """Create a background thread per account.

The thread is responsible for peridoic polling for new mssages and for
evicting cached messages that have expired.

    """
    config = load_config()
    for account in Account.query.all():
        if account.nickname not in zync:
            gmail = NnSync(account.email, account.nickname, config)
            zync[account.nickname] = gmail.sync()
    #atexit.register(kill_zync)

def kill_zync():
    """Stop and wait for all background (polling) threads."""
    print('atexit handler called')
    for nickname in zync:
        thread, egress, ingress = zync[nickname]
        print('%s putting empty command' %nickname)
        egress.put(None)
        while not ingress.empty():
            ingress.get()
        thread.join()


