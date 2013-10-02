#!/usr/bin/env python
import sys
sys.path.append("/usr/local/lib/decades")
from pydecades.database import get_database

def application(environ, start_response):
   conn = get_database()
   cur = conn.cursor()
   cur.execute("""SELECT table_name FROM information_schema.tables 
       WHERE table_schema = 'public'""")
   rows = cur.fetchall()
  
   output = '' 
   for row in rows:
      output = output + row[0] + '\r\n'
   
   start_response('200 OK', [('Content-Type', 'text/plain')])
   yield output

   
      
