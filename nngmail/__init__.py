import click
from flask import Flask, jsonify, make_response
from flask.json import JSONEncoder
from flask_sqlalchemy import SQLAlchemy
import os
import yaml

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
db = SQLAlchemy(app)

from nnsync import NnSync
import nngmail.views

@app.errorhandler(404)
def not_found(error):
    return make_response(jsonify({'error': 'The requested URL was not '
                                  'found on the server.'}), 404)

@app.cli.command('init-db')
def init_db_command():
    """Clear the existing data and create new tables."""
    db.create_all()
    click.echo('Initialized the database.')

@app.cli.command('import')
@click.argument('email', type=click.STRING)
@click.argument('nickname', type=click.STRING)
def import_email(email, nickname):
    """Add a new account to the database and import all the message metadata. 

    email - email address to add
    nickname - nickname for the account
    """
    
    config_file = os.path.normpath(os.path.join(app.root_path, '..',
                                                'data', 'config.yaml'))
    try:
        config = yaml.load(open(config_file, mode='rb'))
    except IOError:
        print('Error: reading config file (%s)' % config_file)
        return
    gmail = NnSync(email, nickname, config)
    gmail.pull()    

app.cli.add_command(init_db_command)
app.cli.add_command(import_email)
