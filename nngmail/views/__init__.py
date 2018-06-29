from nngmail import app

@app.route('/')
def index():
    return 'nngmail proxy server is running\n'

