#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''Flight Manager's console'''
import sys
sys.path.append("/usr/local/lib/decades")

#templating
from jinja2 import Environment, FileSystemLoader

#Standard python modules for config and date/time functions
from ConfigParser import SafeConfigParser
from datetime import datetime, timedelta

import csv

title = '''Flight Manager's console'''

def application(environ, start_response):
   env = Environment(loader = FileSystemLoader('/home/eardkdw/work/decades-rt/live/templates'))
   template = env.get_template('template.html')
   start_response('200 OK', [('Content-Type', 'text/html;charset=utf-8')])
   yield template.render(
         title=title,
         script='''var refreshId = setInterval(function() {
     $('#statcontainer').load('/live/stat.wsgi');
}, 500);''',
         body='<div id="statcontainer"></div>'
            ).encode('utf-8')
