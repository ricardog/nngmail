import os
import yaml

import click
from flask import Flask, jsonify, make_response
from flask.json import JSONEncoder
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy.engine import Engine
from sqlalchemy import event

class MySQLAlchemy(SQLAlchemy):

    def apply_driver_hacks(self, app, info, options):
        super(MySQLAlchemy, self).apply_driver_hacks(app, info, options)
        options['connect_args'] = {'timeout': 15}

class MyJSONEncoder(JSONEncoder):
    def default(self, o):
        ser = getattr(o, 'serialize', None)
        #import pdb; pdb.set_trace()
        if callable(ser):
            return o.serialize()
        return super(MyJSONEncoder, self).default(o)

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///../data/nngmail.sqlite3'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
#app.config['SQLALCHEMY_ECHO'] = True
app.json_encoder = MyJSONEncoder
db = MySQLAlchemy(app)
zync = dict()
get_sync = lambda account: GmSync.from_account(account, sync_config)

from gmsync import GmSync
from nngmail.models import Account, Contact, Thread
from nngmail.api import api_bp
import nngmail.views
import nngmail.background

app.register_blueprint(api_bp)

@event.listens_for(Engine, "connect")
def set_sqlite_pragma(dbapi_connection, connection_record):
    cursor = dbapi_connection.cursor()
    cursor.execute("PRAGMA foreign_keys=ON")
    cursor.execute("PRAGMA synchronous=NORMAL")
    cursor.close()

@app.errorhandler(404)
def not_found(error):
    return make_response(jsonify({'error': 'The requested URL was not '
                                  'found on the server.'}), 404)

@app.errorhandler(403)
def permission_denied(error):
    return make_response(jsonify({'error': error.description}), error.code)

@app.cli.command('init-db')
def init_db_command():
    """Clear the existing data and create new tables."""
    db.create_all()
    Contact.as_unique(db.session, email='no.name@example.com', name='No Name')
    ac = Account.as_unique(db.session, email='no.name@example.com',
                           nickname='no.name')
    db.session.flush()
    Thread.as_unique(db.session, account=ac, thread_id='No thread')
    db.session.commit()
    click.echo('Initialized the database.')

@app.cli.command('import')
@click.argument('email', type=click.STRING)
@click.argument('nickname', type=click.STRING)
@click.option('--init-cache', is_flag=True, default=False)
@click.option('--quiet', '-q', is_flag=True, default=False)
def import_email(email, nickname, init_cache, quiet):
    """Add a new account to the database and import all the message metadata.

    email - email address to add
    nickname - nickname for the account
    """

    gmail = GmSync(email, nickname, load_config(not quiet))
    gmail.pull()
    if init_cache:
        print('fetching cacheable messages')
        gmail.init_cache()

@app.cli.command('resync')
@click.argument('nickname', type=click.STRING)
@click.option('--init-cache', is_flag=True, default=False)
@click.option('--reset', is_flag=True, default=False)
@click.option('--quiet', '-q', is_flag=True, default=False)
def resync_email(nickname, init_cache, reset, quiet):
    """Re-sync message metadata for account.

    nickname - select which account to re-synchronize
    init-cache - fetch recent messages into the cache
    reset - ignore local sync state and do a full resynchronization

    """

    account = Account.query.filter_by(nickname=nickname).one()
    gmail = GmSync.from_account(account, load_config(not quiet))
    if reset:
        gmail.full_pull()
    else:
        gmail.partial_pull()
    if init_cache:
        print('fetching cacheable messages')
        gmail.init_cache()

@app.cli.command('verify')
@click.argument('nickname', type=click.STRING)
@click.option('--quiet', '-q', is_flag=True, default=False)
def verify_email(nickname, quiet):
    """Verify all the message metadata for account.

    nickname - nickname for the account
    """

    account = Account.query.filter_by(nickname=nickname).one()
    gmail = GmSync.from_account(account, load_config(not quiet))
    gmail.verify()

def load_config(verbose=False):
    config_file = os.path.normpath(os.path.join(app.root_path, '..',
                                                'data', 'config.yaml'))
    try:
        config = yaml.load(open(config_file, mode='rb'),
                           Loader=yaml.SafeLoader)
    except IOError:
        print('Error: reading config file (%s)' % config_file)
        return {}
    if verbose:
        config['verbose'] = verbose
    return config

sync_config = load_config()

app.cli.add_command(init_db_command)
app.cli.add_command(import_email)
