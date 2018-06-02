from http.server import BaseHTTPRequestHandler

class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path != '/':
            self.send_error(404, "Object not found")
            return
        self.send_response(200)
        self.send_header('Content-type', 'text/html; charset=utf-8')
        self.end_headers()

        # serve up an infinite stream
        i = 0
        while True:
            self.wfile.write(str.encode("%i " % i))
            time.sleep(0.1)
            i += 1

    def do_POST(self):
        if self.path == '/account':
            self.send_response(200)
            self.send_header('Content-type', 'text/html; charset=utf-8')
            self.end_headers()
            self.wfile.write('<h1>configured</h1>')
        else:
            self.send_response(404, 'Object not found')
            return

