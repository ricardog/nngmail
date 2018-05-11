#!/usr/bin/env python3

import local2
import remote


def main():
  gmail = remote.Gmail()
  sql3 = local.Sqlite3('siarts.db')
  
