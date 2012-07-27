#!/usr/bin/python
import psycopg2
conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")
conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT)


cursor = conn.cursor()

cursor.execute('SELECT %s FROM scratchdata WHERE id=4908' % ('"AERACK01.utc_time"',))
print cursor.query
print(cursor.fetchall())


