#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''An example of using WSGI with the DECADES python library. 
This provides the same functionality as the Java Plot.jar
in Status mode'''
import sys
sys.path.append("/usr/local/lib/decades")

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

#Decades code
from pydecades.database import get_database
from pydecades.rt_calcs import rt_derive

#Standard python modules for config and date/time functions
from ConfigParser import SafeConfigParser
from datetime import datetime, timedelta

def deg_to_dms(deg):
    '''Converts decimal degrees into a degrees/minutes/seconds list'''
    d = int(deg)
    md = abs(deg - d) * 60
    m = int(md)
    sd = (md - m) * 60
    return [d, m, sd]

def application(environ, start_response):
   conn = get_database()
   cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
   calfile = "/usr/local/lib/decades/pydecades/rt_calcs/HOR_CALIB.DAT"
   rtlib = rt_derive.derived(cur, calfile)

   #Basic template for well-formed XHTML output (note '%s' for string subbing later)
   template = '''<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="content-type" content="text/html; charset=utf-8"/>
<title>%s</title>
<style>
body {
   font-family: sans-serif;
   background-color: #186f4d;
   color: white;
}
div#statblock {
   width: 30em;
   padding: 0.75em;
   background-color: white;
   margin-left: auto;
   margin-right: auto;
}
div#statblock p{
   color: #0C2174;
   padding: 0;
   margin-top: 0;
   margin-right: 0;
   margin-bottom: 0.2em;
   margin-left: 0.25em;
}
</style>
</head>
<body>
%s
</body>
</html>'''

 
   #get stat data (taken directly from pydecades/decades_server.py) 
   prtgindata = rtlib.derive_data_alt(['time_since_midnight','utc_time','derindex','flight_number','pressure_height_kft','static_pressure','gin_latitude','gin_longitude','gin_heading'], '=id','ORDER BY id DESC LIMIT 1')
   #get corcon separately so gin/prt stuff is independant of it.
   corcondata = rtlib.derive_data_alt(['time_since_midnight','utc_time','derindex','true_air_speed', 'deiced_true_air_temp_c','dew_point','gin_wind_speed','wind_angle'], '=id','ORDER BY id DESC LIMIT 1')

   #open output div 
   output = '<div id="statblock">' 
   if(corcondata['time_since_midnight'] and abs(corcondata['derindex'] - prtgindata['derindex']) < 10):
      output = output + '<p>Flight %s %s</p>' % (prtgindata['flight_number'][0],datetime.utcfromtimestamp(prtgindata['utc_time']).strftime('%H:%M:%SZ'))
      output = output + ('<p>Heading %.0f° Speed %.0fkts Height %.0fkft Pressure %.0fmb</p>' % (prtgindata['gin_heading'],corcondata['true_air_speed'], prtgindata['pressure_height_kft'], prtgindata['static_pressure']))
      output = output + ('<p>Lat %.0f°%.0f\'%.2f" Long %.0f°%.0f\'%.2f" Wind %.1fms¯¹ / %.0f°</p>' % tuple(deg_to_dms(prtgindata['gin_latitude']) + deg_to_dms(prtgindata['gin_longitude']) + [corcondata['gin_wind_speed'],corcondata['wind_angle']] ))
      output = output + ('<p>Temp %.1f°C Dewpoint %.1f°C</p>' % (corcondata['deiced_true_air_temp_c'],corcondata['dew_point']))
   elif (prtgindata['time_since_midnight']):
      output = '<p>Flight %s %s</p>' % (prtgindata['flight_number'][0],datetime.utcfromtimestamp(prtgindata['utc_time']).strftime('%H:%M:%SZ'))
      output = output + ('<p>Heading %.0f° Speed %.0fkts Height %.0fkft Pressure %.0fmb</p>' % (prtgindata['gin_heading'],float('NaN'), prtgindata['pressure_height_kft'], prtgindata['static_pressure']))
      output = output + ('<p>Lat %.0f°%.0f\'%.2f" Long %.0f°%.0f\'%.2f" Wind %.1fms¯¹ / %.0f°</p>' % tuple(deg_to_dms(prtgindata['gin_latitude']) + deg_to_dms(prtgindata['gin_longitude']) + [float('NaN'),float('NaN')] ))
      output = output + ('<p>Temp %.1f°C Dewpoint %.1f°C</p>' % (float('NaN'),float('NaN')))
   else:
      output = '<p>Flight ####</p>' 
   #close output div
   output = output + '</div>'
   
   start_response('200 OK', [('Content-Type', 'application/xhtml+xml;charset=utf-8')])
   yield template % ('DECADES Status', output)

   
      
