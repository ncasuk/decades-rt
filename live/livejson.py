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
   def GET(self):
      '''Usage: via web.py, e.g. http://fish/live/livejson'''
      web.header('Content-Type','application/json; charset=utf-8', unique=True) 
      web.header('Cache-control', 'no-cache')
      conn = get_database()
      cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      parser = DecadesConfigParser()
      calfile = parser.get('Config','calfile')
      rtlib = rt_derive.derived(cur, calfile)

      #get data
      data = rtlib.derive_data_alt(['time_since_midnight','utc_time','pressure_height_m','static_pressure','gin_latitude','gin_track_angle','gin_longitude','gin_heading','gin_d_velocity','gin_altitude','gin_speed', 'true_air_speed', 'deiced_true_air_temp_c','dew_point','gin_wind_speed','wind_angle'], '=id','ORDER BY id DESC LIMIT 1')
      #each entry is a length=1 list, so flatten
      keylist = data.keys()
      for each in keylist:
         if np.isnan(data[each][0]):#don't return NaNs
            del data[each] 
         else:
            data[each] = data[each][0] 

      #data['utc_time'] = datetime.fromtimestamp(data['utc_time'],timezone('utc')).strftime('%H:%M:%S') 
      #Javascript time is in whole milliseconds
      data['javascript_time'] = data['utc_time'] * 1000 
      return json.dumps(data, allow_nan=False) #in *no particular order*

      