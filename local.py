#!/usr/bin/env python

import sqlite3

class Local:
    "Store data read from gmail API to sqlite."

    conn = None
    
    def __init__(self, fname):
        self.fname = fname

    def connected(func):
        "Ensure connection to DB is open.."
        def func_wrap (self, *args, **kwargs):
            if self.conn is None:
                self.open()
            return func(self, *args, **kwargs)
        return func_wrap

    def open(self):
        "Create a new sqlite3 object / connection."
        self.conn = sqlite3.connect(self.fname)

    @connected
    def create_tables(self):
        "Create the tables used to store synchronization information."
        cursor = self.conn.cursor()
        cursor.execute('CREATE TABLE IF NOT EXISTS mapping ' +
                       '(id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, ' +
                       'gid VARCHAR(100) UNIQUE NOT NULL)')
        cursor.execute('CREATE INDEX IF NOT EXISTS gid_idx ON mapping (gid)')
        cursor.execute('CREATE TABLE IF NOT EXISTS kv ' +
                       '(key TEXT UNIQUE NOT NULL, value TEXT NOT NULL)')
        self.conn.commit()

    @connected
    def set_kv(self, key, value):
        "Store a key, value pair in the DB."
        cursor = self.conn.cursor()
        cursor.execute('REPLACE INTO kv (key, value) VALUES (?,?)', (key, value))
        self.conn.commit()

    @connected
    def get_kv(self, key):
        "Read a key, value pair from the DB.  Raises KeyError if they key is 
        not found."
        cursor = self.conn.cursor()
        cursor.execute('SELECT value FROM kv WHERE key = ?', (key,))
        item = cursor.fetchone()
        if item is None:
            raise KeyError(key)
        return item[0]

    @connected
    def insert_gid(self, gid):
        "Insert a gId into the mapping table."
        query = '''
INSERT INTO mapping (gid)
SELECT ?
WHERE NOT EXISTS (SELECT 1 
                  FROM mapping 
                  WHERE gid = ?)
'''.format(what = gid)
        cursor = self.conn.cursor()
        cursor.execute(query, (gid, gid))
        self.conn.commit()

def main():
    me = Local('storage.sqlite3')
    me.create_tables()
    me.set_kv('lastHistoryId', 0)
    for gid in ('0xfoobar', '0xfoomanchu', '0xcafebabe', '0xcafebabe'):
        me.insert_gid(gid)
    for hid in (1, 2, 3, 4):
        print('last history ID: %s' % me.get_kv('lastHistoryId'))
        me.set_kv('lastHistoryId', hid)
    print('last history ID: %s' % me.get_kv('lastHistoryId'))

if __name__ == '__main__':
    main()
