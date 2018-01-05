#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''produces a Parano.txt-type file on-the-fly based on the
Display Parameters csv file '''
import web

urls = (
   #'', 'parano'
   '\.(.+)', 'parameters'  #DO NOT CALL THIS CLASS 'parano'
                           #it doesn't work. May be meaningful
                           #elsewhere? - DW 2014-06-02
)
app= web.application(urls, locals())   

#Standard python modules for config and date/time functions
from pydecades.configparser import DecadesConfigParser
from datetime import datetime
import csv, operator, json
from pydecades.rt_calcs.rt_derive import derived
from pydecades.database import get_database 
import psycopg2.extras

class parameters:
   def GET(self, filetype):
      conn = get_database()
      cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      parser = DecadesConfigParser()
      calfile = parser.get('Config','calfile')
      rtlib = derived(cur, calfile)
      '''parameters_file = parser.get('Config','parameters_file')
      #read CSV display parameters file
      with open(parameters_file, 'r') as csvfile:
         parameters = csv.DictReader(csvfile)   #uses first line as fieldnames
         sortedparams = sorted(parameters, key=operator.itemgetter('DisplayText'))
      '''
      parameters=rtlib.get_paranos()
      sortedparams = sorted(parameters.itervalues(), key=operator.itemgetter('ParameterIdentifier'))
      if filetype == 'txt': 
         output = 'PARANO.TXT - Derived parameter numbers for HORACE                      ' + datetime.utcnow().strftime('%d/%m/%Y') + '\n'
         output = output + ''' 
   Numbers start at 513, so there is no confusion with DRS parameter numbers.
   DECADES has a known list of numbers -> method names.
   Autogenerated from rt_derive.py \n\n'''
   
      
         for line in sortedparams:             
            output = output + '{0:<6s}{1:<7s}{2:<12s}{3:<29s}{4:s}\n'.format(line['ParameterIdentifier'],'('+str(int(line['ParameterIdentifier'])-512)+')','####',line['DisplayText'],(line['DisplayUnits']).strip('()'))
   
         return output

      elif filetype == 'json':
         web.header('Content-Type','application/json; charset=utf-8', unique=True) 
         web.header('Cache-control', 'no-cache')

         parameters['utc_time'] = {'DisplayText': 'Time', 'DisplayUnits': 'UTC', 'ParameterName':'utc_time','GroupId':'all'}
         return json.dumps(parameters) 

      elif filetype == 'raw':
         web.header('Content-Type','application/json; charset=utf-8', unique=True) 
         web.header('Cache-control', 'no-cache')

         parameters=rtlib.get_raw_paranos()
         parameters['utc_time'] = {'DisplayText': 'Time', 'DisplayUnits': 'UTC', 'ParameterName':'utc_time','GroupId':'all'}
         return json.dumps(parameters) 


      else: #you want what?
			raise web.notfound()