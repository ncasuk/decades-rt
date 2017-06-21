#!/usr/bin/python
'''Listens for requests from the Java client applet and responds appropriately'''
from array import array

from twisted.internet import protocol, reactor
from twisted.application import internet, service
from twisted.protocols import basic
from twisted.python import log

import psycopg2 
import psycopg2.extras
from collections import namedtuple # so we can used namedtuple cursors

from sys import stdout
import time
import math
from datetime import datetime, timedelta
import struct
from rt_calcs import rt_derive,rt_data
from pydecades.configparser import DecadesConfigParser
import numpy as np
from pydecades.database import get_database

#class to handle Decades events
class DecadesProtocol(basic.LineReceiver):
   '''Python version of the HORACE server - to work with DECADES system. Responds to two commands:

       * ``STAT`` (returns basic status data e.g. lat/long, heading etc.)
       * ``PARA`` (Returns requested parameters)'''

   delimiter = "" #Java DatInputStream does not have a delimiter between lines
   der = []
   def __init__(self,rtlib,parano):
       '''Takes a database connection, and creates a NamedTuple cursor (allowing us to
         access the results by fieldname *or* index'''
       log.msg("DECADES_server")
       self.parano = parano
       self.rtlib=rtlib
         
      
   def connectionMade(self):
      log.msg("connection from", self.transport.getPeer())
      self.setRawMode()

   def lineReceived(self, line):
      log.msg(line)

   def rawDataReceived(self, data):
      '''Deals with incoming requests from the HORACE java applet - ignores
         any requests not starting with STAT or PARA'''
      if data[0:4] == "STAT":
         self.writeStatus()
      if data[0:4] == "PARA":
         formt = ">4s" + str((len(data)/4)-1) + 'i' # 4 characters (P A R A), start time integer, end time integer, integer indicating number of parameters, then integer codes for the parameters (see horaceplot/choices/PARANO.TXT)
         para = struct.unpack(formt,data)
         #log.msg("Incoming: " +  repr(para))
         self.writeStatus()
         #send integer: index = utc_time
         self.sendLine(struct.pack(">i",self.rtlib.status['utc_time']))
         
       
         paralist = []
         for paracode in para[4:(4+para[3])]:
            paralist.append(self.parano[paracode])

         if len(paralist) == 0:
             #paralist.append('utc_time') #so that the no-data request returns correctly
             returndata={}

         elif para[2] == -1:
             #it wants all up to latest data point
             returndata = self.rtlib.derive_data_alt(paralist, 'utc_time>= %i' % para[1])
         else:
             #in this case there is a specific range it wants
             returndata = self.rtlib.derive_data_alt(paralist, 'utc_time BETWEEN %i AND %i' % (para[1],para[2]))
         #Calculate size if returned data - make all parameter the same
         size_upcoming=0
         for each in returndata:
             if(len(returndata[each])>size_upcoming):
                 size_upcoming=len(returndata[each])
         for each in returndata:
             if(len(returndata[each])<size_upcoming):
                 print 'missing some %s' % each
                 returndata[each]=np.empty((size_upcoming,))
                 returndata[each].fill(np.nan)

         #send integer of size of upcoming data
         self.sendLine(struct.pack(">i",size_upcoming))
         #log.msg('requesting data between %i and %i, returning %i datapoints' % (para[1],para[2],size_upcoming))
         #send each requested parameter separately
         for paracode in para[4:(4+para[3])]: #list of required fields
               sendable = returndata[self.parano[paracode]]
               for each in sendable:
                  self.sendLine(struct.pack(">f",each))
            


   def writeStatus(self):
      self.sendLine(self.rtlib.get_packed_status())    
               

class DecadesFactory(protocol.ServerFactory):
   _recvd = {}
   def __init__(self):
      self.protocol = DecadesProtocol
      conn=get_database()
      parser = DecadesConfigParser()
      self.parano={}
      #for (code, function) in parser.items('Parameters'):
      #    self.parano[int(code)] = function
      calfile = parser.get('Config','calfile')
      cursor = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
      self.rtlib=rt_derive.derived(cursor,calfile) #class processing the cals & producing "real" values
      for k,v in self.rtlib.get_paranos().iteritems():
          self.parano[int(v['ParameterIdentifier'])]=k
      print 'Init factory'
   
   def buildProtocol(self,addr):
      p = self.protocol(self.rtlib,self.parano)
      p.factory = self
      return p


def main():# Listen for TCP:1500
   log.startLogging(stdout)
   reactor.listenTCP(1500, DecadesFactory())
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported
