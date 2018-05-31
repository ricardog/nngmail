import click
from nngmail.nngmail import app, db
from nngmail.nngmail.models import *

@app.route('/')
def index():
    return 'Hello World!'

@app.route('/account/')
def account():
    click.echo("getting accounts")
    click.echo("there are %d accounts" % Account.query.count())
    return '\n'.join(map(lambda a: str(a), Account.query.all()))

