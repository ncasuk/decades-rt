from pydecades.decades_server import DecadesFactory, DecadesProtocol
from pydecades.database import get_database
from pydecades.decades import DecadesDataProtocols
from twisted.trial import unittest
from twisted.test import proto_helpers
from twisted.internet import defer
import testresources

from numpy import isnan, nan, array
import psycopg2 
import psycopg2.extras

import os, struct, exceptions, csv

from datetime import datetime

from twisted.python import log
from pydecades.configparser import DecadesConfigParser

class GenerateTestDecadesFactory(testresources.TestResourceManager):
   '''Generates a Factory which is reused for every test case, as would
   be the case when used live. The primary advantage is that the config files
   (e.g. ``/etd/decades/decades.ini`` and 
   ``/etc/decades/Display_Parameters_ver1.3.csv``) are only read once''' 
   def make(self, dependency_resources):

      print "Generating Factory"
      #create test database
      self.ddp = DecadesDataProtocols()
      self.conn = get_database(parser)
      self.cursor = self.conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      self.ddp.create_maintable(self.cursor)
      with open('../pydecades/test/sample-data-ascension.csv') as sample_data:
        reader = csv.DictReader(sample_data)
        all_fields = self.ddp.all_fields()
        for row in reader:
            #strip test data that's not in current dataformats
            line = {k: row[k] for k in list(set(all_fields.keys()) & set(row.keys()))}
            for each in line:
                if line[each] == '':
                    if all_fields[each] == 'real':
                        line[each] = '10.0' #dummy value
                    elif all_fields[each] == 'integer':
                        line[each] = '10' #dummy value
                    else:
                        line[each] = 'NULL'
                elif all_fields[each] == 'varchar':
                    line[each] = "'" + line[each] + "'"
                elif all_fields[each] == 'boolean':
                    if(line[each] == 't'):
                        line[each] = 'TRUE'
                    else:
                        line[each] = 'FALSE'
                        
            
            line['utc_time'] =  row['utc_time']
            self.cursor.execute("INSERT INTO mergeddata (" + ", ".join(line.keys()) + ") VALUES (" + ", ".join(line.values()) +")")

      factory = DecadesFactory(parser=parser)
      return factory
   
class DecadesProtocolTestCase(unittest.TestCase, testresources.ResourcedTestCase):
   '''Unit tests for the Decades Protocol class (i.e. all the parameter functions
   plus the responses to Horace-style ``PARA`` and ``STAT`` calls'''

   timeout = 240

   resources = [
      ('factory', GenerateTestDecadesFactory()),
   ]

   def setUp(self):
      ''' Sets up the test case, reusing existing Factory if available'''
      super(DecadesProtocolTestCase, self).setUp()

      self.proto = self.factory.buildProtocol('128.0.0.1')
      self.tr = proto_helpers.StringTransport()
      self.proto.makeConnection(self.tr)

   def tearDown(self):
      self.tr.loseConnection()

   def test_stat(self):
    
      self.proto.rawDataReceived('STAT') #"transmits" STAT command
      #Assumes successful if it unpacks correctly
      self.assertTrue(struct.unpack(self.proto.rtlib.status.struct_fmt, self.tr.value()))
      

   def _test_para(self,param_id,function):
      #requests all extant data, for now. 
      self.proto.rawDataReceived(struct.pack(">4s5i", 'PARA',-1,-1,2,515,param_id))
      data = self.tr.value()
      self.assertEqual(15,len(struct.unpack(self.proto.rtlib.status.struct_fmt, data[0:57])),msg=function + " does not return the correct size of data from a STAT request") #PARA requests return a 57-byte STAT respose 
      (derindex, size_upcoming) = struct.unpack('>2i',data[57:65])
      #Assert returned data are of the correct length (57 bytes is length of STAT block)
      #2 parameters so "2 * size_upcoming" is the size of the datablock 
      para_fmt = '>ii'+str(2*size_upcoming)+'f'
      self.assertEqual(len(data)-57,struct.calcsize(para_fmt), msg=function + " does not return the correct size of data from a PARA request")

      out = struct.unpack(para_fmt, data[57:])
      #check is not returning NaNs
      self.assertFalse(isnan(out[2+size_upcoming:]).any(), msg=function + " returning NaNs. Function missing in rt_derive?")

   def test_flight_number(self):
      self.proto.rawDataReceived('STAT') #"transmits" STAT command
      flight = struct.unpack(self.proto.rtlib.status.struct_fmt, self.tr.value())[-1]
      self.assertTrue(type(flight) == str)
      self.assertTrue(len(flight) == 4)
      self.assertTrue(flight == 'C037')

   def test_derive_data_alt(self):
      statblock= self.proto.rtlib.derive_data_alt(['time_since_midnight','utc_time','flight_number','pressure_height_kft','static_pressure','gin_latitude','gin_longitude','gin_heading'], '','DESC LIMIT 1')
      print type(statblock['flight_number'])
      print u'<p>Flight {}  {}</p>'.format(statblock['flight_number'][0], datetime.utcfromtimestamp(statblock['utc_time']).strftime('%H:%M:%SZ'))


#Not part of testcase class; is a function that returns a function based on parameters
def test_parameters(param_id, function):
   def arbitrary_parameter_function(self):
      self._test_para(int(param_id), function)
   return arbitrary_parameter_function

#create one test function per Parameter entry in decades.ini
parser = DecadesConfigParser()
parser.set('Database','user', parser.get('Database','user') + '_test')
parser.set('Database','database', parser.get('Database','database') + '_test')
for (code, function) in parser.items('Parameters'):
   if code != "513": #flight_number is not called for plotting, and would fail as it doesn't return a float
      test_method = test_parameters(code, function)
      test_method.__name__ = 'test_%s' % function 
      setattr (DecadesProtocolTestCase, test_method.__name__, test_method)
