#!/usr/bin/env python
# vim: set fileencoding=utf-8:

'''Graphing data for Javascript/jQuery/flot based UI'''

import web
from render_helper import render_template

urls = (
    '/cartesian', 'cartesian',
    '/tephigram', 'tephigram'
)
app= web.application(urls, locals())   

#Standard python modules for config and date/time functions
from pydecades.configparser import DecadesConfigParser
from datetime import datetime, timedelta
from dateutil import parser
from pytz import timezone
from time import mktime
import json

class cartesian:
   def GET(self):
           
      #defaults to from now
      now = int(mktime(datetime.utcnow().timetuple()))
      #HTML standard colours (except for white)
      colours = ['aqua', 'black', 'blue', 'fuchsia', 'gray', 'green', 'lime', 'maroon', 'navy', 'olive', 'orange', 'purple', 'red', 'silver', 'teal', 'yellow']
      user_data = web.input(x="javascript_time",y=["deiced_true_air_temp_c"],frm=None,to=None,c=colours)
      if user_data.frm is not None and user_data.frm is not '':
         try:
            frm_epoch = int(mktime(parser.parse(user_data.frm + ' UTC').timetuple()))
         except (AttributeError, ValueError):
            frm_epoch = now 
      else:
         frm_epoch = now

      if user_data.to is not None and user_data.to is not '':
         try: 
            to_epoch = int(mktime(parser.parse(user_data.to + ' UTC').timetuple()))
         except (AttributeError, ValueError):
            to_epoch = ''
      else:
         to_epoch = ''
      return render_template('chart-cartesian.html', x=user_data.x, y=user_data.y, c=user_data.c,frm_epoch=frm_epoch, to_epoch=to_epoch,colours=colours)
      #return '<h1>' + user_data.y0 + '</h1>'
           
'''Draws tephigrams'''
class tephigram:
   def GET(self):
      
      #defaults to from now
      now = int(mktime(datetime.utcnow().timetuple()))
      #HTML standard colours (except for white)
      colours = ['aqua', 'black', 'blue', 'fuchsia', 'gray', 'green', 'lime', 'maroon', 'navy', 'olive', 'orange', 'purple', 'red', 'silver', 'teal', 'yellow']
      user_data = web.input(x="static_pressure",y=["deiced_true_air_temp_c","dew_point"],frm=None,to=None,c=colours)
      if user_data.frm is not None and user_data.frm is not '':
         try:
            frm_epoch = int(mktime(parser.parse(user_data.frm + ' UTC').timetuple()))
         except (AttributeError, ValueError):
            frm_epoch = now 
      else:
         frm_epoch = now

      if user_data.to is not None and user_data.to is not '':
         try: 
            to_epoch = int(mktime(parser.parse(user_data.to + ' UTC').timetuple()))
         except (AttributeError, ValueError):
            to_epoch = ''
      else:
         to_epoch = ''
      return render_template('chart-tephigram.html', x=user_data.x, y=user_data.y, c=user_data.c,frm_epoch=frm_epoch, to_epoch=to_epoch,colours=colours)
