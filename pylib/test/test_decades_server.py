from pylib.decades_server import DecadesFactory, DecadesProtocol
from pylib.database import get_database
from twisted.trial import unittest
from twisted.test import proto_helpers
import os, struct

class DecadesProtocolTestCase(unittest.TestCase):
   def setUp(self):
      print os.getcwd()
      factory = DecadesFactory(get_database(), "../pylib/rt_calcs/HOR_CALIB.DAT")
      self.proto = factory.buildProtocol('127.0.0.1')
      self.tr = proto_helpers.StringTransport()
      self.proto.makeConnection(self.tr)

   #def _test(self, string, expected):

   def test_stat(self):
      self.proto.rawDataReceived('STAT')
      #Assumes successful if it unpacks correctly
      self.assertTrue(struct.unpack(self.proto.status_struct_fmt, self.tr.value()))
