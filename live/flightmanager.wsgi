#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''Flight Manager's console'''
#templating
from jinja2 import Environment, FileSystemLoader

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

from pydecades.configparser import DecadesConfigParser
from pydecades.rt_calcs import rt_derive
from pydecades.database import get_database

#Standard python modules for config and date/time functions
from datetime import datetime, timedelta

import csv

title = '''Flight Manager's console'''

def application(environ, start_response):
   conn = get_database()
   cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
   parser = DecadesConfigParser()
   output_dir = parser.get('Config','output_dir')
   calfile = parser.get('Config','calfile')
   rtlib = rt_derive.derived(cur, calfile)

   #get calibrated data from DB as required
   results = rtlib.derive_data_alt(['time_since_midnight','utc_time','flight_number','pressure_height_kft'],'=id','ORDER BY id DESC LIMIT 1')
   
   env = Environment(loader = FileSystemLoader(environ['JinjaTemplates']))
   template = env.get_template('template.html')
   start_response('200 OK', [('Content-Type', 'text/html;charset=utf-8')])
   yield template.render(
         title=title,
         script='''
$(document).ready(function() {
   $('#actionselect').change(
      function() {
         $('#action').val($('#actionselect').val());
      }
)
});
var refreshId = setInterval(function() {
     $('#statcontainer').load('/live/stat.wsgi');
}, 1000);''',
         body='<div id="statcontainer"></div>' +
           '''
<div id="flightsummary">
   <form name="add_summary">
      <select id="actionselect">
         <option>
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
