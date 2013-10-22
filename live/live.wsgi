#!/usr/bin/env python
import web
import sys
sys.path.append('/var/www/decades-live')
import status, flight, parano
from render_helper import render_template

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

#To get server data
import platform, os, os.path, glob
from pydecades.decades import DecadesDataProtocols
from pydecades.configparser import DecadesConfigParser
from pydecades.rt_calcs import rt_derive
from pydecades.database import get_database

#to encode
import json

urls = (
   '/index', 'index',
   '/stat', status.app,
   '/parano', parano.app,
   '/flight', flight.app,   
   '/tank_status\.(.*)', 'tank_status'
)
app = web.application(urls, globals(), autoreload=False)
application = app.wsgifunc()


class index:        
    def GET(self):
        web.header('Content-Type','text/html; charset=utf-8', unique=True) 
        return render_template('index.html',
           title='DECADES on ' + platform.node(),
        ).encode('utf-8')

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
        statuses['Tank']['name'] = platform.node()
        #get PTPD status
        try:
            with open('/var/log/ptpd/ptpd-stats.log','r') as ptpdstats:
               statuses['Tank']['PTPD'] = list(ptpdstats)[-1]
        except IOError:
            #No PTPD statlog
            statuses['Tank']['PTPD'] = None;
            
        results = self.rtlib.derive_data_alt(['flight_number'],'=id','ORDER BY id DESC LIMIT 1')
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
            for line in latest:
               l = line.split(': ')
               if l[1].strip() == 'MISSING':
                  statuses['TCP'][l[0]] = ['MISSING',None, None]
               else:
                  fileinfo = os.stat(l[1].strip())
                  statuses['TCP'][l[0]] = [l[1].strip(), fileinfo.st_mtime, fileinfo.st_size]
            
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
               title=platform.node() + 'Tank Status ',
               statuses=statuses,
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
            web.notfound()
            
         
            
      



if __name__ == "__main__":
    app.run()

