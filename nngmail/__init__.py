import click
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
import yaml

app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///../data/nngmail.sqlite3'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
db = SQLAlchemy(app)

from nnsync import NnSync
import nngmail.views

@app.cli.command('init-db')
def init_db_command():
    """Clear the existing data and create new tables."""
    db.create_all()
    click.echo('Initialized the database.')

@app.cli.command('import')
@click.argument('email', type=click.STRING)
@click.argument('config_file', type=click.File(mode='rb'))
def import_email(email, config_file):
    print('config file      : %s' % config_file.name)
    config = yaml.load(config_file)
    gmail = NnSync(email, config)
    gmail.pull()    

app.cli.add_command(init_db_command)
app.cli.add_command(import_email)
