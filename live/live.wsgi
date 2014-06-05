#!/usr/bin/env python
import web
import sys
sys.path.append('/var/www/decades-live')
import status, flight, parano, avaps, livejson
from render_helper import render_template

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

#To get server data
import platform, os, os.path, glob, subprocess
from pydecades.decades import DecadesDataProtocols
from pydecades.configparser import DecadesConfigParser
from pydecades.rt_calcs import rt_derive
from pydecades.database import get_database

from datetime import datetime
from pytz import timezone

#to encode
import json
import csv, operator

urls = (
   '/index', 'index',
   '/stat', status.app,
   '/avaps', avaps.app,
   '/livejson', livejson.app,
   '/parano', parano.app,
   '/flight', flight.app,   
   '/tank_status\.(.*)', 'tank_status',
   '/livegraph', 'livegraph'
)
app = web.application(urls, globals(), autoreload=False)
application = app.wsgifunc()

#web.config.debug = False

class index:        
    def GET(self):
        web.header('Content-Type','text/html; charset=utf-8', unique=True) 
        return render_template('index.html',
           title=' ' + platform.node(),
        )

class tank_status:
   dataProtocols = DecadesDataProtocols() 
   parser = DecadesConfigParser()
   def __init__(self):
      self.conn = get_database()
      self.cur = self.conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      self.db = web.database(dbn='postgres',host=self.parser.get('Database','host'), db=self.parser.get('Database','database'), user=self.parser.get('Database','user'), pw=self.parser.get('Database','password'))

      self.output_dir = self.parser.get('Config','output_dir')
      self.calfile = self.parser.get('Config','calfile')
      self.rtlib = rt_derive.derived(self.cur, self.calfile)

   def GET(self,filetype):
        statuses = {} #dictionary
        #tank name (e.g. fish or septic)
        statuses['Tank'] =  {}
        statuses['Tank']['Name'] = platform.node()
        statuses['Tank']['Temp'] = subprocess.check_output("sensors")
        #get PTPD status
        try:
            with open('/var/log/ptpd/ptpd-stats.log','r') as ptpdstats:
               statuses['Tank']['PTPD'] = list(ptpdstats)[-1]
        except IOError:
            #No PTPD statlog
            statuses['Tank']['PTPD'] = None;
            
        results = self.rtlib.derive_data_alt(['time_since_midnight','flight_number','static_pressure'],'=id','ORDER BY id DESC LIMIT 1')
        try:
            statuses['Tank']['Flight'] = str(results['flight_number'][0])
        except IndexError:
            pass #ignore it, it's not been set yet
        

        statuses['Process'] = {}
        #DECADES twistd services (check PID in pidfiles is running)
        for each in glob.glob('/var/run/decades*.pid*'):
            f = open(each, 'r')
            pid = f.readlines()
            statuses['Process'][os.path.basename(each).replace('.pid','')] = os.path.exists('/proc/' + pid[0])
        statuses['TCP'] = {}
        with open(os.path.join(self.output_dir,'latest'),'r') as latest:
            latest_array = json.load(latest)
            for each in latest_array:
               if latest_array[each] != 'MISSING':
                  fileinfo = os.stat(latest_array[each])
                  statuses['TCP'][each] = [latest_array[each], fileinfo.st_mtime, fileinfo.st_size]
               else:
                  statuses['TCP'][each] = ['MISSING', None, None]
            
        #UDP data
        statuses['UDP'] = {}
        for each in self.dataProtocols.available():
            #of form twcdat01_utc_time 
            field = each.lower() + '_utc_time'
            #gets most recent UTC Time field from that console
            entries = self.db.select('mergeddata', {}, where='mergeddata.' + field + ' IS NOT NULL', what=field, order='mergeddata.id DESC', limit=1)
            statuses['UDP'][each] = None
            for entry in entries: #should only be 0 or 1 entries
               statuses['UDP'][each] = dict(entry)[field]
                        
        if filetype == 'json':
            web.header('Content-Type', 'text/plain')
            return json.dumps(statuses)
        elif filetype == 'html':
            web.header('Content-Type','text/html; charset=utf-8', unique=True) 
            return render_template('tank_status.html',
               title=platform.node() + ' Tank Status',
               statuses=statuses,
               curtime=float(datetime.now(timezone('utc')).strftime('%s')),
               warning_s=30.0,
               critical_s=60.0
            ).encode('utf-8')
        elif filetype in ['txt','ini']:
            web.header('Content-Type', 'text/plain')
            output = ''
            for section in statuses:
               output = output  + '[' +section+']' +'\r\n'
               for key in statuses[section]:
                   output = output  + '   ' + key + ': ' + str(statuses[section][key]) +'\r\n'
            
            return output
         
        else:
            raise web.notfound()

class livegraph:
   def GET(self):
      '''parser = DecadesConfigParser()
      parameters_file = parser.get('Config','parameters_file')
      params = '';
      #read CSV display parameters file
      with open(parameters_file, 'r') as csvfile:
         parameters = csv.DictReader(csvfile)   #uses first line as fieldnames
         sortedparams = sorted(parameters, key=operator.itemgetter('DisplayText'))
         for line in sortedparams:
            params = params + '<option value="' + line['ParameterName'] + '">' + line['DisplayText'] + ' ' + (line['DisplayUnits']).strip('()') + '</option>'
      '''
            
      user_data = web.input(x="javascript_time",y=["deiced_true_air_temp_c"])
      #HTML standard colours (except for white)
      colours = ['aqua', 'black', 'blue', 'fuchsia', 'gray', 'green', 'lime', 'maroon', 'navy', 'olive', 'orange', 'purple', 'red', 'silver', 'teal', 'yellow']
      return render_template('livegraph.html', x=user_data.x, y=user_data.y, colours=colours)
      #return '<h1>' + user_data.y0 + '</h1>'
            
         
if __name__ == "__main__":
    app.run()

