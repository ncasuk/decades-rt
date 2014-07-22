#!/usr/bin/env python
# vim: set fileencoding=utf-8:
# vim: set ts=3:
# vim: set et:
'''Translates a supplied EDT file from a sonde into JSON of the 
same form as livejson.py, so it can be displayed (e.g. on a 
tephigram)''' 

import web

urls = (
   '(.*)', 'sondejson'
)

app= web.application(urls, locals())

from datetime import datetime, timedelta
from pytz import timezone
import csv, json

class sondejson:
   '''loads an EDT file, returns contents as JSON'''
   filename = '/home/eardkdw/radiosonde-data-20130911_120120.tsv'

   def GET(self,parano=False):
      '''loads EDT file, processes it, returs result'''
      web.header('Content-Type','application/json; charset=utf-8', unique=True)
      web.header('Cache-control', 'no-cache')

      infile = open(self.filename,'rb')
      #skip 4 lines 
      for i in range(1,5):
         infile.readline()
      
      mapinfo ={}
      f=infile.readline()
      while f != '\n':
         element= f.split(':')
         mapinfo[element[0].strip()] = element[1].strip()
         f=infile.readline()

      #skip blank lines
      while f == '\n':
         f=infile.readline()

      #f now contains the field names for the datafield descriptions
      datainfo_fields = map(str.strip,f.split(':'))
      f=infile.readline()
      if all(c in ' -\n' for c in f): #i.e contains only spaces,hyphens,LF
         f=infile.readline() #ditch it by loading the next line

      #load datainfo
      datainfo = []
      while f != '\n':
         datainfo.append(f)
         f=infile.readline()
      
      datainfo_reader = csv.DictReader(datainfo,fieldnames=datainfo_fields,delimiter=' ',skipinitialspace=True)
      data_desc = []
      datainfo_json = {}
      datafields = []
      for row in datainfo_reader:
         data_desc.append(row)
         '''make JSON parano-type output (of form: {"DisplayUnits": "(counts)", "Tooltip": null, "DisplayText": "CORE STATIC SENSOR", "Visible?": null, "ParameterName": "prtaft01_jci140_signal", "ParameterIdentifier": "700", "GroupID": null, "Icon": null}'''
         datainfo_json[row['Record name']] = {"DisplayUnits": row['Unit'], "Tooltip": False, "DisplayText": row['Record name'], "Visible?": False, "ParameterName": row['Record name'], "ParameterIdentifier":False, "GroupID": False, "Icon": False}
         datafields.append(row['Record name'])
         

      #skip spacer lines and seperators
      while all(c in ' *\n' for c in f):
         point = infile.tell()
         f=infile.readline() #ditch it by loading the next line

      #back one line
      infile.seek(point)

      #we're doing all to the end now
      data_reader = csv.DictReader(infile,fieldnames=datafields, delimiter='\t', skipinitialspace=True)
      out =[]
      for row in data_reader:
         #there's a trailing tab for some reason
         del row[None]
         #cast values as floats
         for field in datafields:
            row[field] = float(row[field])
         for temp in ['T','TD']: #temperatures
            row[temp]  = row[temp] - 273.15
            
         out.append(row)
      
      if(parano == ".parameters"):
         return json.dumps(datainfo_json) 
      else:
         return json.dumps(out)

      #return out 
