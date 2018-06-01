#!/usr/bin/env python3

import click
from http.server import BaseHTTPRequestHandler, HTTPServer
import os
import socket
import threading
import time
import yaml

import nngmail

import pdb

## Based on the following SO question:
## https://stackoverflow.com/questions/46210672/python-2-7-streaming-http-server-supporting-multiple-connections-on-one-port

# Launch 100 listener threads.
class HTTPServerThread(threading.Thread):
    def __init__(self, i, sock):
        threading.Thread.__init__(self)
        self.i = i
        self.daemon = True
        self.sock = sock
        self.server_close = lambda self: None
        self.start()
        self.accounts = {}

    def run(self):
        httpd = HTTPServer(addr, Handler, False)

        # Prevent the HTTP server from re-binding every handler.
        # https://stackoverflow.com/questions/46210672/
        httpd.socket = self.sock
        httpd.server_bind = lambda self: None

        httpd.serve_forever()

@click.command()
@click.argument('config_file', type=click.File(mode='rb'))
def daemon(config_file):
    """Start a daemon to synchronize email accounts as specified by the
config file (config-file).

    """

    print('daemon starting')
    working_dir = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
    os.chdir(working_dir)
    print("working directory: %s" % working_dir)
    print('config file      : %s' % config_file.name)
    config = yaml.load(config_file)
    addr = ('localhost', 8000)
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(addr)
    sock.listen(5)
    [HTTPServerThread(i, sock) for i in range(5)]

    if None:
        gmail = nngmail.NnGmail(config)
        gmail.pull()
        msgs = gmail.read(range(2140, 2150))
        time.sleep(10)
    print('daemon shutting down')

def test():
    # Create ONE socket.
    addr = ('localhost', 8000)
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(addr)
    sock.listen(5)
    [HTTPServerThread(i, sock) for i in range(5)]
    time.sleep(9e9)

if __name__ == '__main__':
    test()
