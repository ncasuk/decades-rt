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

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

from pydecades.configparser import DecadesConfigParser
from pydecades.rt_calcs import rt_derive
from pydecades.database import get_database

#Standard python modules for config and date/time functions
from datetime import datetime, timedelta
from pytz import timezone

import csv, os

title = '''Flight Manager's console'''

def latitude(value):
   value=float(value)
   return ("%.2f" % abs(value)) + ('S' if value <0 else 'N')

def longitude(value):
   value=float(value)
   return ("%.2f" % abs(value)) + ('W' if value <0 else 'E')

def render_template(template_name, **context):
    extensions = context.pop('extensions', [])
    globals = context.pop('globals', {})

    jinja_env = Environment(
            loader=FileSystemLoader(os.path.join(os.path.dirname(__file__), 'templates')),
            extensions=extensions,
            )
    jinja_env.filters['latitude'] = latitude
    jinja_env.filters['longitude'] = longitude
    jinja_env.globals.update(globals)

    #jinja_env.update_template_context(context)
    return jinja_env.get_template(template_name).render(context)

class flightmanager:
   def __init__(self):
      self.parser = DecadesConfigParser()

      self.conn = get_database()
      self.cur = self.conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      self.db = web.database(dbn='postgres',host=self.parser.get('Database','host'), db=self.parser.get('Database','database'), user=self.parser.get('Database','user'), pw=self.parser.get('Database','password'))

      self.output_dir = self.parser.get('Config','output_dir')
      self.calfile = self.parser.get('Config','calfile')
      self.rtlib = rt_derive.derived(self.cur, self.calfile)
   
   def GET(self):
      web.header('Content-Type','text/html; charset=utf-8', unique=True) 

      #get calibrated data from DB as required
      results = self.rtlib.derive_data_alt(['time_since_midnight','utc_time','flight_number','pressure_height_kft'],'=id','ORDER BY id DESC LIMIT 1')

      #get existing summary entries
      entries = self.db.select('summary', {'flight':results['flight_number'][0] }, where='summary.flight_number = $flight')
      
       
      return render_template('flightmanager.html',
            title=title,
            entries=entries,
            script='''
   $(document).ready(function() {
      $('#actionselect').change(
         function() {
            $('#event').val($('#actionselect').val());
         }
   )
   });
   var refreshId = setInterval(function() {
      $('#statcontainer').load('/live/stat');
   }, 1000);'''
       ).encode('utf-8')
   
   def POST(self):
      #get details of POSTed form
      action = web.input()
     
      #get location & time data 
      prtgindata = self.rtlib.derive_data_alt(['time_since_midnight','utc_time','flight_number','pressure_height_kft','gin_latitude','gin_longitude','gin_heading'], '=id','ORDER BY id DESC LIMIT 1')
     
      #uses fromtimestamp rather than utcfromtimestamp as we want the result to be TZ-aware 
      with self.db.transaction():
         if(action.submit=='start'):
            db_res = self.db.insert('summary', flight_number=prtgindata['flight_number'][0], start=datetime.fromtimestamp(prtgindata['utc_time'], timezone('utc')), start_heading=int(prtgindata['gin_heading']), start_latitude=float(prtgindata['gin_latitude']), start_longitude=float(prtgindata['gin_longitude']), start_height=float(prtgindata['pressure_height_kft']), comment=action.comment, event=action.event, finished=(action.status == 'instant'))
         elif(action.submit=='stop' and action.id):
            db_res = self.db.update('summary', 'summary.id=$id', {'id':action.id},stop=datetime.fromtimestamp(prtgindata['utc_time'], timezone('utc')),stop_heading=int(prtgindata['gin_heading']), stop_latitude=float(prtgindata['gin_latitude']), stop_longitude=float(prtgindata['gin_longitude']), stop_height=float(prtgindata['pressure_height_kft']), finished=True)
        
      #reload page via GET
      raise web.seeother('')
