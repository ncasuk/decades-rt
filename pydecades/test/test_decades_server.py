from pydecades.decades_server import DecadesFactory, DecadesProtocol
from pydecades.database import get_database
from twisted.trial import unittest
from twisted.test import proto_helpers
import testresources

from numpy import isnan, nan

import os, struct, exceptions

from twisted.python import log
from pydecades.configparser import DecadesConfigParser

class GenerateTestDecadesFactory(testresources.TestResourceManager):
   '''Generates a Factory which is reused for every test case, as would
   be the case when used live. The primary advantage is that the config files
   (e.g. ``/etd/decades/decades.ini`` and 
   ``/etc/decades/Display_Parameters_ver1.3.csv``) are only read once''' 
   def make(self, dependency_resources):
      print "Generating Factory"
      return DecadesFactory()
   
class DecadesProtocolTestCase(testresources.ResourcedTestCase):
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
      self.assertFalse(isnan(out[2:]).any(), msg=function + " returning NaNs")

#Not part of testcase class; is a function that returns a function based on parameters
def test_parameters(param_id, function):
   def arbitrary_parameter_function(self):
      self._test_para(int(param_id), function)
   return arbitrary_parameter_function

#create one test function per Parameter entry in decades.ini
parser = DecadesConfigParser()
for (code, function) in parser.items('Parameters'):
   if code != "513": #flight_number is not called for plotting, and would fail as it doesn't return a float
      test_method = test_parameters(code, function)
      test_method.__name__ = 'test_%s' % function 
      setattr (DecadesProtocolTestCase, test_method.__name__, test_method)
