#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''An example of using WSGI with the DECADES python library. 
This provides the same functionality as the Java Plot.jar
in Status mode by default, but can return any requested data'''

import web
import re

urls = (
    '', 'jsondata'
)
app= web.application(urls, locals())   

#Libraries to access the PostgreSQL database
import psycopg2 
import psycopg2.extras
import numpy as np
import numpy.ma as ma
from collections import namedtuple # so we can used namedtuple cursors

#Decades code
from pydecades.database import get_database
from pydecades.rt_calcs import rt_derive,rt_data

#Standard python modules for config and date/time functions
from pydecades.configparser import DecadesConfigParser
from datetime import datetime, timedelta
from pytz import timezone
import json

class jsondata:
   '''Produces the requested data as JSON for live display'''
   conn = get_database()
   cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
   parser = DecadesConfigParser()
   calfile = parser.get('Config','calfile')
   rtlib = rt_derive.derived(cur, calfile)
   d=datetime.strftime(datetime.now(),'%H:%M:%S')
   def GET(self):
      '''Usage: via web.py, e.g. http://fish/live/livejson'''
      try:
          web.header('Content-Type','application/json; charset=utf-8', unique=True) 
          web.header('Cache-control', 'no-cache')
          user_data = web.input(para=[])
      except AttributeError:
          print "probably run from command line"
          print
          class ud(dict):
              def __init__(self, *args):
                  dict.__init__(self, args)
                  self.para=[]
         
          user_data = ud()

      if len(user_data.para) == 0:
         #no paras sent, send default
         return self.rtlib.get_json_status()
      else:
         parameters = filter(None,user_data.para) #strips empty entries

      '''if 'javascript_time' in parameters:
         parameters.remove('javascript_time')   #strips javascript time'''
                                                #as it is computed below.
      #conditions = '=id '
      #orderby = 'ORDER BY id DESC LIMIT 1'
      conditions = ''
      orderby = ' DESC LIMIT 1'
      last_time=self.rtlib.derive_data(['utc_time'],conditions,orderby)['utc_time']
      if user_data.has_key('to') and user_data.to > '':
         try:
            #sanitise (coerce to INT)
            to = int(user_data.to)
            conditions = 'utc_time <=%s ' % to 
            orderby = ''
         except ValueError:
            #can't be converted to integer, ignore
            pass;
      else:
         conditions = 'utc_time <%i ' % last_time

      if user_data.has_key('frm') and user_data.frm > '':
         try:
            #sanitise (coerce to INT)
            frm = int(user_data.frm)
            if(conditions): conditions+=' AND '
            conditions += ' utc_time >=%s ' % frm 
            orderby = '' 
         except ValueError:
            #can't be converted to integer, ignore
            pass;

    
         #get data
      paras=set(parameters+['utc_time'])
      data=self.rtlib.derive_data( paras, conditions,orderby)
      #dataout = {'start':self.d}
      dataout={}
      for each in data.keys():
          if(each=='utc_time'):
              if(each in parameters):
                  dataout[each]=(data[each].astype('int64')).tolist()
              else:
                  try:
                      dataout[each]=[(data[each][-1].astype('int64')).item()]
                  except IndexError:
                      dataout[each]=[]
          else:
              try:
                  dataout[each]=ma.masked_invalid(data[each]).tolist()
              except TypeError:
                  dataout[each]=data[each].tolist()
      return json.dumps(dataout)
