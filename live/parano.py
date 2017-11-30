#!/usr/bin/env python
# vim: set fileencoding=utf-8:
'''produces a Parano.txt-type file on-the-fly based on the
Display Parameters csv file '''
import web

urls = {
   '', 'parano'
}
app= web.application(urls, locals())   

#Standard python modules for config and date/time functions
from pydecades.configparser import DecadesConfigParser
from datetime import datetime
import csv

class parano:
   def GET(self):
	   parser = DecadesConfigParser()
	   parameters_file = parser.get('Config','parameters_file')
	   output = 'PARANO.TXT - Derived parameter numbers for HORACE                      ' + datetime.utcnow().strftime('%d/%m/%Y') + '\n'
	   output = output + ''' 
   Numbers start at 513, so there is no confusion with DRS parameter numbers.
   DECADES has a known list of numbers -> method names.
   Autogenerated from ''' + parameters_file + '\n\n'
   
	   
	   #read CSV display parameters file
	   with open(parameters_file, 'r') as csvfile:
		   parameters = csv.DictReader(csvfile)	#uses first line as fieldnames
		   for line in parameters:
			   output = output + '{0:<6s}{1:<7s}{2:<12s}{3:<29s}{4:s}\n'.format(line['ParameterIdentifier'],'('+str(int(line['ParameterIdentifier'])-512)+')','####',line['DisplayText'],(line['DisplayUnits']).strip('()'))
   
	   
	   return output
