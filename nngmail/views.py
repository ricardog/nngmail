import click
from nngmail import app, db
from nngmail.models import *

@app.route('/')
def index():
    return 'Hello World!'

@app.route('/account/')
def account():
    click.echo("getting accounts")
    click.echo("there are %d accounts" % Account.query.count())
    return '\n'.join(map(lambda a: str(a), Account.query.all())) + '\n'

@app.route('/contact/')
def contact():
    click.echo("getting contacts")
    click.echo("there are %d contacts" % Contact.query.count())
    return '\n'.join(map(lambda a: str(a), Contact.query.limit(100).all())) + '\n'


