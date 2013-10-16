#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''Flight Manager's console'''
import web
#templating
from jinja2 import Environment,FileSystemLoader

urls = {
   '', 'flightmanager'
}

app= web.application(urls, locals())   
db = web.database(dbn='postgres', user='username', pw='password', db='dbname')

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

from pydecades.configparser import DecadesConfigParser
from pydecades.rt_calcs import rt_derive
from pydecades.database import get_database

#Standard python modules for config and date/time functions
from datetime import datetime, timedelta

import csv, os

title = '''Flight Manager's console'''
def render_template(template_name, **context):
    extensions = context.pop('extensions', [])
    globals = context.pop('globals', {})

    jinja_env = Environment(
            loader=FileSystemLoader(os.path.join(os.path.dirname(__file__), 'templates')),
            extensions=extensions,
            )
    jinja_env.globals.update(globals)

    #jinja_env.update_template_context(context)
    return jinja_env.get_template(template_name).render(context)

class flightmanager:
   def GET(self):
      web.header('Content-Type','text/html; charset=utf-8', unique=True) 
      conn = get_database()
      cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      parser = DecadesConfigParser()
      output_dir = parser.get('Config','output_dir')
      calfile = parser.get('Config','calfile')
      rtlib = rt_derive.derived(cur, calfile)

      #get calibrated data from DB as required
      results = rtlib.derive_data_alt(['time_since_midnight','utc_time','flight_number','pressure_height_kft'],'=id','ORDER BY id DESC LIMIT 1')

      #get existing summary entries
      entries = db.select('summary', where='summary.flight_number = '.results['flight_number'][0])
       
      return render_template('flightmanager.html',
            title=title,
            entries=entries,
            script='''
   $(document).ready(function() {
      $('#actionselect').change(
         function() {
            $('#action').val($('#actionselect').val());
         }
   )
   });
   var refreshId = setInterval(function() {
      $('#statcontainer').load('/live/stat');
   }, 1000);''',
            body='<div id="statcontainer"></div>' +
            '''
   <div id="flightsummary">
      <form name="add_summary">
         <select id="actionselect">
            <option />
            <option>Run</option>
            <option>Profile</option>
            <option>Orbit</option>
         </select>
         <input type="text" size="20" id="action" />
         <select name="status" onChange="optione();">
            <option>Start Action</option>
            <option>Stop Action</option>
            <option>Start new action and stop previous action</option>
            <option>Action</option>
         </select>
         <label for="comment">Comment</label>
         <input type="text" size="30" id="comment" name="comment" />
         <input type="button" name="event" value="submit" />
      </form>
   </div>'''
       ).encode('utf-8')
