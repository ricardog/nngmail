from nngmail import app

@app.route('/')
def index():
    return 'Hello World!\n\n'

