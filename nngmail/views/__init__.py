from flask import jsonify

from nngmail import app
from nngmail.models import Contact

@app.route('/')
def index():
    return 'Hello World!\n\n'

