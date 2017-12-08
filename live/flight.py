#!/usr/bin/env python
# vim: set fileencoding=utf-8:
# vim: set tabstop=3: set expandtab
'''Flight Manager's console'''
import web

urls = {
   "/(.+)", 'flight'
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

import csv

from cStringIO import StringIO

from render_helper import render_template, latitude, longitude

title = '''Flight Manager's console'''

class flight:
   def __init__(self):
      self.parser = DecadesConfigParser()

      self.conn = get_database()
      self.cur = self.conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      self.db = web.database(dbn='postgres',host=self.parser.get('Database','host'), db=self.parser.get('Database','database'), user=self.parser.get('Database','user'), pw=self.parser.get('Database','password'))

      self.output_dir = self.parser.get('Config','output_dir')
      self.calfile = self.parser.get('Config','calfile')
      self.rtlib = rt_derive.derived(self.cur, self.calfile)
   
   def GET(self,path):
      '''Displays current flight summary entries and form to add more'''

		#do explicit check that path is one of expected values 
      if path in ('manager', 'summary', 'events','csv'): 
         #get calibrated data from DB as required
         results = self.rtlib.derive_data_alt(['time_since_midnight','utc_time','flight_number','pressure_height_kft'],'','DESC LIMIT 1')
         flight_num = self.rtlib.getdata_fromdatabase('corcon01_flight_num', '', 'DESC LIMIT 1')

         #get existing summary entries
         entries = self.db.select('summary', {'flight_number':flight_num[0] }, where='summary.flight_number = $flight_number', order='summary.start DESC')
     
         web.header('Cache-control', 'no-cache')
         if path == 'csv': #CSV outputs as a file to download
            web.header('Content-Type', 'text/csv')
            web.header('Content-Disposition', 'attachment;filename=FLTSUM01' + datetime.utcnow().strftime('_%Y%m%d_%H%M%S_') + '{}-summary.csv'.format( flight_num[0]))

            csv_file = StringIO()
            csv_writer = csv.writer(csv_file)
            csv_writer.writerow(['Event','Start','Start Hdg / °','Start Hgt / kft','Start Lat / °','Start Long / °', 'Stop', 'Stop Hdg / °','Stop Hgt / kft','Stop Lat / °',' Stop Long / °', 'Comment'])
            for entry in sorted(entries, key=lambda e: e.start): #sorts by start
               csv_writer.writerow([entry.event, entry.start, entry.start_heading, entry.start_height, entry.start_latitude, entry.start_longitude, entry.stop, entry.stop_heading, entry.stop_height, entry.stop_latitude, entry.stop_longitude, entry.comment])
            return csv_file.getvalue()
         else: #the rest are all HTML and v. similar
            web.header('Content-Type','text/html; charset=utf-8', unique=True) 
            return render_template('flight'+path+'.html',
               title=title,
               entries=entries,
            ).encode('utf-8')
      else: #not an accepted path, so 404
			raise web.notfound()
   
   def POST(self,path):
      '''Takes POSTed variables, processes them and returns a HTTP 303 See Other status. 
         (i.e. uses the PRG Pattern see: http://en.wikipedia.org/wiki/Post/Redirect/Get )'''
      #only for manager
      if path != 'manager':
         raise web.notfound()
      #get details of POSTed form
      action = web.input()
     
      #get location & time data 
      prtgindata = self.rtlib.derive_data_alt(['time_since_midnight','utc_time','flight_number','pressure_height_kft','gin_latitude','gin_longitude','gin_heading'], '','DESC LIMIT 1')
      flight_num = self.rtlib.getdata_fromdatabase('corcon01_flight_num', '', 'DESC LIMIT 1')
     
      #uses fromtimestamp rather than utcfromtimestamp as we want the result to be TZ-aware 
      with self.db.transaction():
         if(action.submit=='start'):
            if(action.exclusive == 'True'):
               #only one exclusive event can run at a time 
               #get ids of other open exclusive events
               unfinished_exclusives = self.db.select('summary', {'exclusive':True, 'finished':False, 'ongoing':True, 'flight_number':flight_num[0]}, where='exclusive=$exclusive AND ongoing=$ongoing AND flight_number=$flight_number AND finished=$finished', what='id' )
               #close them
               for unfinished in unfinished_exclusives:
                  self._stop(unfinished.id, prtgindata)
            #start new action
            db_res = self.db.insert('summary', flight_number=flight_num[0], start=datetime.fromtimestamp(prtgindata['utc_time'], timezone('utc')), start_heading=int(prtgindata['gin_heading']), start_latitude=float(prtgindata['gin_latitude']), start_longitude=float(prtgindata['gin_longitude']), start_height=float(prtgindata['pressure_height_kft']), comment=action.comment, event=action.event, ongoing=(action.status == 'ongoing'), finished=(action.status == 'instant'), exclusive=(action.exclusive == 'True'))
         elif(action.submit=='stop' and action.id):
               
            self._stop(action.id, prtgindata)
        
      #reload page (PRG pattern)
      raise web.seeother(web.ctx.homedomain + web.ctx.homepath + web.ctx.path,absolute=True)

   def _stop(self, actionid, prtgindata):
      '''Stops & finishes an unfinished entry in the summary table'''
      db_res = self.db.update('summary', 'summary.id=$id', {'id':int(actionid)},stop=datetime.fromtimestamp(prtgindata['utc_time'], timezone('utc')),stop_heading=int(prtgindata['gin_heading']), stop_latitude=float(prtgindata['gin_latitude']), stop_longitude=float(prtgindata['gin_longitude']), stop_height=float(prtgindata['pressure_height_kft']), finished=True)
      
