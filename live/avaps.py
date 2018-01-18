#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''An example of using WSGI with the DECADES python library. 
This provides the same functionality as the Java Plot.jar
in Status mode'''

import web

urls = (
    '', 'avaps'
)
app= web.application(urls, locals())   

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

#Decades code
from pydecades.database import get_database
from pydecades.rt_calcs import rt_derive

#Standard python modules for config and date/time functions
from pydecades.configparser import DecadesConfigParser
from datetime import datetime, timedelta
from pytz import timezone
import json

class avaps:
   '''Produces the data required by the NCAR AVAPS system as JSON for conversion
      later  (see NCAR AVAPS Interface Control Document: 
      http://www.eol.ucar.edu/isf/facilities/dropsonde/AVAPS_Interface_RevC.html )'''
   def GET(self):
      '''Usage: via web.py, e.g. http://fish/live/avaps'''
      web.header('Content-Type','text/plain', unique=True) 
      web.header('Cache-control', 'no-cache')
      conn = get_database()
      cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      parser = DecadesConfigParser()
      calfile = parser.get('Config','calfile')
      rtlib = rt_derive.derived(cur, calfile)

      #get data
      data = rtlib.derive_data(['time_since_midnight','utc_time','pressure_height_m','static_pressure','gin_latitude','gin_track_angle','gin_longitude','gin_heading','gin_d_velocity','gin_altitude','gin_speed', 'true_air_speed', 'deiced_true_air_temp_c','dew_point','gin_wind_speed','wind_angle'], '',' DESC LIMIT 1')
      #each entry is a length=1 list, so flatten
      for each in data:
         data[each] = data[each][0]

      data['utc_time'] = datetime.fromtimestamp(data['utc_time'],timezone('utc')).strftime('%H:%M:%S') 
      del data['time_since_midnight'] #as that is needed by derive_data_alt but not in teh return

      return json.dumps(data) #in *no particular order*

      
