#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''An example of using WSGI with the DECADES python library. 
This provides the same functionality as the Java Plot.jar
in Status mode'''

import web

urls = (
    '', 'livejson'
)
app= web.application(urls, locals())   

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
import numpy as np
from collections import namedtuple # so we can used namedtuple cursors

#Decades code
from pydecades.database import get_database
from pydecades.rt_calcs import rt_derive

#Standard python modules for config and date/time functions
from pydecades.configparser import DecadesConfigParser
from datetime import datetime, timedelta
from pytz import timezone
import json

class livejson:
   '''Produces the requested data as JSON for live display'''
   default = ['pressure_height_m','static_pressure','gin_latitude','gin_track_angle','gin_longitude','gin_heading','gin_d_velocity','gin_altitude','gin_speed', 'true_air_speed', 'deiced_true_air_temp_c','dew_point','gin_wind_speed','wind_angle']
   always = ['time_since_midnight','utc_time']
   def GET(self):
      '''Usage: via web.py, e.g. http://fish/live/livejson'''
      web.header('Content-Type','application/json; charset=utf-8', unique=True) 
      web.header('Cache-control', 'no-cache')
      conn = get_database()
      cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      parser = DecadesConfigParser()
      calfile = parser.get('Config','calfile')
      rtlib = rt_derive.derived(cur, calfile)
      user_data = web.input(para=[])
      if len(user_data.para) == 0:
         #no paras sent, send default
         parameters = self.default
      else:
         parameters = filter(None,user_data.para) #strips empty entries

      if 'javascript_time' in parameters:
         parameters.remove('javascript_time')   #strips javascript time
                                                #as it is computed below.
      conditions = ''
      orderby = ' DESC LIMIT 1'
      if user_data.has_key('to') and user_data.to > '':
         try:
            #sanitise (coerce to INT)
            to = int(user_data.to)
            conditions = ' utc_time <=%s ' % to 
            orderby = ''
         except ValueError:
            #can't be converted to integer, ignore
            pass;

      if user_data.has_key('frm') and user_data.frm > '':
         try:
            #sanitise (coerce to INT)
            frm = int(user_data.frm)
            if(conditions): conditions+=' AND '
            conditions = conditions + ' utc_time >=%s ' % frm 
            orderby = ' LIMIT 36000' #i.e. 10 hrs
         except ValueError:
            #can't be converted to integer, ignore
            pass;
     
         #get data
      data = rtlib.derive_data_alt(self.always + parameters, conditions,orderby)
      keylist = data.keys()
      #loop over records, make each record self-contained
      dataout = []
      for n in range(0, len(data[keylist[0]])): 
         dataout.append({})
         for each in keylist:
            if not(np.isnan(data[each][n])):#don't return NaNs
               dataout[n][each] = data[each][n] 
            else:
               del dataout[n];
               break; #go on to next entry
         #Javascript time is in whole milliseconds
         dataout[n]['javascript_time'] = dataout[n]['utc_time']*1000.0

      #data['utc_time'] = datetime.fromtimestamp(data['utc_time'],timezone('utc')).strftime('%H:%M:%S') 
      return json.dumps(dataout, allow_nan=False) #in *no particular order*

      
