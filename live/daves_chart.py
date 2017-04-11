#!/usr/bin/env python
# vim: set fileencoding=utf-8:

'''Graphing data for Javascript/jQuery/flot based UI'''

import web
from render_helper import render_template

urls = (
    '/xy', 'xy',
    '/tephi', 'tephigram'
      
)
app= web.application(urls, locals())   

#Standard python modules for config and date/time functions
from pydecades.configparser import DecadesConfigParser
from datetime import datetime, timedelta
from dateutil import parser
from pytz import timezone
from time import mktime
import json
colours = ['aqua', 'black', 'blue', 'fuchsia', 'gray', 'green', 'lime', 'maroon', 'navy', 'olive', 'orange', 'purple', 'red', 'silver', 'teal', 'yellow']

class xy:
   def GET(self,x=[],y=[],frm='',to='',c=colours):
           
      #defaults to from now
      now = int(mktime(datetime.utcnow().timetuple()))
      #HTML standard colours (except for white)
      user_data = web.input(x=[],y=[],frm='',to='',c=colours)

      if hasattr(user_data.x,'lower'):
          x=[str(user_data.x)]
      else:
          x=[str(f) for f in user_data.x]
      if(len(x)>0):defx=x
      if hasattr(user_data.y,'lower'):
          user_data.y=[str(user_data.y)]
      else:
          y=[str(f) for f in user_data.y]
      if(len(y)>0):defy=y
      frm_epoch=now
      if user_data.frm:
         try:
            frm_epoch = int(mktime(parser.parse(user_data.frm).utctimetuple()))
            if(frm_epoch>now):
                frm_epoch-=86400
         except (AttributeError, ValueError):
            pass
      to_epoch = ''
      if user_data.to:
         try: 
            to_epoch = int(mktime(parser.parse(user_data.to + ' UTC').timetuple()))
            if(to_epoch>now):
                to_epoch-=86400
         except (AttributeError, ValueError):
            pass
      mult=''
      if x==['utc_time']:
         mult='*1000'
      if(len(x)>1):
          indp=y
          dep=x
          paras=zip(dep,dep,indp*len(dep))
      else:
          indp=x
          dep=y
          paras=zip(dep,indp*len(dep),dep)
      allparas=x+y
      if(len(x)==0):x=['utc_time']
      if(len(y)==0):y=['deiced_true_air_temp_c']
      return render_template('daves-chart.html', x=x, y=y, 
                             c=user_data.c,frm_epoch=frm_epoch, to_epoch=to_epoch,
                             frm=user_data.frm,to=user_data.to,
                             colours=colours,mult=mult,paras=paras,allparas=allparas,indp=indp,dep=dep)
           
'''Draws tephigrams'''
class tephigram:
   def GET(self):
      
      #defaults to from now
      now = int(mktime(datetime.utcnow().timetuple()))
      #HTML standard colours (except for white)
      user_data = web.input(x="static_pressure",y=["deiced_true_air_temp_c","dew_point"],frm=None,to=None,c=colours)
      if user_data.frm is not None and user_data.frm is not '':
         try:
            frm_epoch = int(mktime(parser.parse(user_data.frm + ' UTC').timetuple()))
            if(frm_epoch>now):
                frm_epoch-=86400
         except (AttributeError, ValueError):
            frm_epoch = now 
      else:
         frm_epoch = now

      if user_data.to is not None and user_data.to is not '':
         try: 
            to_epoch = int(mktime(parser.parse(user_data.to + ' UTC').timetuple()))
            if(to_epoch>now):
                to_epoch-=86400
         except (AttributeError, ValueError):
            to_epoch = ''
      else:
         to_epoch = ''
      return render_template('daves-tephigram.html', x=user_data.x, y=user_data.y, c=user_data.c,frm_epoch=frm_epoch, to_epoch=to_epoch,colours=colours)
