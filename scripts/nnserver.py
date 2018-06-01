#!/usr/bin/env python3

import click
import server.nnserver as nnserver

@click.command()
@click.argument('config_file', type=click.File(mode='rb'))
def daemon(config_file):
    """Start a daemon to synchronize email accounts as specified by the
config file (config-file).

    """
    nnserver.daemon(config_file)

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
