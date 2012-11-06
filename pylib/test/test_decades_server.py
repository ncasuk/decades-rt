from pylib.decades_server import DecadesFactory, DecadesProtocol
from pylib.database import get_database
from twisted.trial import unittest
from twisted.test import proto_helpers
import os, struct

from twisted.python import log

class DecadesProtocolTestCase(unittest.TestCase):
   def setUp(self):
      factory = DecadesFactory(get_database(), "../pylib/rt_calcs/HOR_CALIB.DAT")
      self.proto = factory.buildProtocol('127.0.0.1')
      self.tr = proto_helpers.StringTransport()
      self.proto.makeConnection(self.tr)

   def test_stat(self):
      self.proto.rawDataReceived('STAT') #"transmits" STAT command
      #Assumes successful if it unpacks correctly
      self.assertTrue(struct.unpack(self.proto.status_struct_fmt, self.tr.value()))

   def test_para(self):
      #requests all extant data, for now. 
      self.proto.rawDataReceived(struct.pack(">4s5i", 'PARA',-1,-1,2,515,521));
      data = self.tr.value()
      self.assertEqual(18,len(struct.unpack(self.proto.status_struct_fmt, data[0:53]))) #PARA requests return a 53-byte STAT respose 
      (derindex, size_upcoming) = struct.unpack('>2i',data[53:61])
      #Assert returned data are of the correct length (53 bytes is length of STAT block)
      #2 parameters so "2 * size_upcoming" is the size of the datablock 
      self.assertEqual(len(data)-53,struct.calcsize('>ii'+str(2*size_upcoming)+'f'))
