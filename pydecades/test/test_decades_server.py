from pydecades.decades_server import DecadesFactory, DecadesProtocol
from pydecades.database import get_database
from twisted.trial import unittest
from twisted.test import proto_helpers
import os, struct, exceptions

from twisted.python import log
from ConfigParser import SafeConfigParser

class DecadesProtocolTestCase(unittest.TestCase):
   def setUp(self):
      factory = DecadesFactory(get_database(), "../pydecades/rt_calcs/HOR_CALIB.DAT")
      self.proto = factory.buildProtocol('128.0.0.1')

   def test_stat(self):
      tr = proto_helpers.StringTransport()
      self.proto.makeConnection(tr)
      self.proto.rawDataReceived('STAT') #"transmits" STAT command
      #Assumes successful if it unpacks correctly
      self.assertTrue(struct.unpack(self.proto.status_struct_fmt, tr.value()))

      

   def _test_para(self,param_id,function):
      #requests all extant data, for now. 
      tr = proto_helpers.StringTransport()
      self.proto.makeConnection(tr)
      self.proto.rawDataReceived(struct.pack(">4s5i", 'PARA',-1,-1,2,515,param_id))
      data = tr.value()
      self.assertEqual(18,len(struct.unpack(self.proto.status_struct_fmt, data[0:53])),msg=function + " does not return the correct size of data from a STAT request") #PARA requests return a 53-byte STAT respose 
      (derindex, size_upcoming) = struct.unpack('>2i',data[53:61])
      #Assert returned data are of the correct length (53 bytes is length of STAT block)
      #2 parameters so "2 * size_upcoming" is the size of the datablock 
      self.assertEqual(len(data)-53,struct.calcsize('>ii'+str(2*size_upcoming)+'f'), msg=function + " does not return the correct size of data from a PARA request")

#Not part of testcase class; is a function that returns a function based on parameters
def test_parameters(param_id, function):
   def arbitrary_parameter_function(self):
      self._test_para(int(param_id), function)
   return arbitrary_parameter_function

#create one test function per Parameter entry in decades.ini
parser = SafeConfigParser()
config = parser.read(['/etc/decades/decades.ini','pydecades/decades.ini'])
for (code, function) in parser.items('Parameters'):
   if code != "513": #flight_number is not called for plotting, and would fail as it doesn't return a float
      test_method = test_parameters(code, function)
      test_method.__name__ = 'test_%s' % function 
      setattr (DecadesProtocolTestCase, test_method.__name__, test_method)
