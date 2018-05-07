#!/usr/bin/env python

import sqlite3

def open_db(fname):
    conn = sqlite3.connect(fname)
    return conn

def create_tables(conn):
    cursor = conn.cursor()
    cursor.execute('CREATE TABLE IF NOT EXISTS mapping ' +
                   '(id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, ' +
                   'gid VARCHAR(100) UNIQUE NOT NULL)')
    cursor.execute('CREATE INDEX IF NOT EXISTS gid_idx ON mapping (gid)')
    cursor.execute('CREATE TABLE IF NOT EXISTS kv ' +
                   '(key TEXT UNIQUE NOT NULL, value TEXT NOT NULL)')
    conn.commit()

def set_kv(conn, key, value):
    cursor = conn.cursor()
    cursor.execute('REPLACE INTO kv (key, value) VALUES (?,?)', (key, value))
    conn.commit()

def get_kv(conn, key):
    cursor = conn.cursor()
    cursor.execute('SELECT value FROM kv WHERE key = ?', (key,))
    item = cursor.fetchone()
    if item is None:
        raise KeyError(key)
    return item[0]

def insert_gid(conn, gid):
    query = '''
INSERT INTO mapping (gid)
SELECT ?
WHERE NOT EXISTS (SELECT 1 
                  FROM mapping 
                  WHERE gid = ?)
'''.format(what = gid)
    #print(query)
    cursor = conn.cursor()
    cursor.execute(query, (gid, gid))
    conn.commit()

def main():
    conn = open_db('storage.sqlite3')
    create_tables(conn)
    set_kv(conn, 'lastHistoryId', 0)
    for gid in ('0xfoobar', '0xfoomanchu', '0xcafebabe', '0xcafebabe'):
        insert_gid(conn, gid)
    for hid in (1, 2, 3, 4):
        print('last history ID: %s' % get_kv(conn, 'lastHistoryId'))
        set_kv(conn, 'lastHistoryId', hid)
    print('last history ID: %s' % get_kv(conn, 'lastHistoryId'))

if __name__ == '__main__':
    main()
