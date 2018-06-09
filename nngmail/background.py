import atexit
import os
import yaml

from nngmail import app, db, zync
from nngmail.models import Account
from nnsync import NnSync

def load_config():
    config_file = os.path.normpath(os.path.join(app.root_path, '..',
                                                'data', 'config.yaml'))
    try:
        return yaml.load(open(config_file, mode='rb'))
    except IOError:
        print('Error: reading config file (%s)' % config_file)
        return {}

@app.before_first_request
def background():
    config = load_config()
    for account in Account.query.all():
        if account.nickname not in zync:
            gmail = NnSync(account.email, account.nickname, config)
            zync[account.nickname] = gmail.sync()

def kill_zync():
    print('atexit handler called')
    for nickname in zync:
        thread, egress, ingress = zync[nickname]
        print('%s putting empty command' %nickname)
        egress.put(None)
        while not ingress.empty():
            ingress.get()
        thread.join()

atexit.register(kill_zync)

