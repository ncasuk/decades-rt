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

#class to handle Decades events
class DecadesProtocol(basic.LineReceiver):
   '''Python version of the HORACE server - to work with DECADES system. Responds to two commands:
         STAT (returns basic status data e.g. lat/long, heading etc.)
         PARA (Returns requested parameters)'''
   delimiter = "" #Java DatInputStream does not have a delimiter between lines
   der = []
   def __init__(self, conn, status, calfile="pydecades/rt_calcs/HOR_CALIB.DAT"):
       '''Takes a database connection, and creates a NamedTuple cursor (allowing us to
         access the results by fieldname *or* index'''
       log.msg("DECADES_server")
       self.cursor = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
       self.rtlib = rt_derive.derived(self.cursor,calfile) #class processing the cals & producing "real" values
       self.parser = DecadesConfigParser()
       self.status=status
       self.parano = {}
       for (code, function) in self.parser.items('Parameters'):
         self.parano[int(code)] = function
         
      
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
         #log.msg(">iiffff",-1,para[3],self.time_seconds_past_midnight(),274,self.time_seconds_past_midnight(),274)
         #send integer: current max time-index
         self.sendLine(struct.pack(">i",self.status['derindex']))
         
       
         paralist = []
         for paracode in para[4:(4+para[3])]:
            paralist.append(self.parano[paracode])

         if len(paralist) == 0:
            paralist.append('id') #so that the no-data request returns correctly

         if para[2] == -1:
            #it wants all up to latest data point
            returndata = self.rtlib.derive_data_alt(paralist, '>= %i' % para[1])
         else:
            #in this case there is a specific range it wants
            returndata = self.rtlib.derive_data_alt(paralist, 'BETWEEN %i AND %i' % (para[1],para[2]))
        
         #log.msg(self.cursor.query) 
         #returndata = self.cursor.fetchall()
         #send integer of size of upcoming data
         size_upcoming=0
         if(len(para) > 4): #i.e. if any parameters were requested
            size_upcoming = len(returndata[self.parano[para[4]]])
         self.sendLine(struct.pack(">i",size_upcoming))
         #log.msg(repr(len(returndata)))
         #log.msg('requesting data between %i and %i, returning %i datapoints' % (para[1],para[2],size_upcoming))
         #send each requested parameter separately
         for paracode in para[4:(4+para[3])]: #list of required fields
               sendable = returndata[self.parano[paracode]]
               for each in sendable:
                  self.sendLine(struct.pack(">f",each))
            

         #log.msg(msg)
         #self.derindex = self.derindex +1
         #log.msg('\x00@\x00\x00\x00@@\x00\x00')
         #self.sendLine('\x00@\x00\x00\x00@@\x00\x00')

   def writeStatus(self):
      #log.msg('STATUS',hex(id(self.status)))
      self.status.checkStatus(self.rtlib)
      self.sendLine(self.status.packed())    
               

class DecadesFactory(protocol.ServerFactory):
   _recvd = {}
   def __init__(self, conn,  calfile):
      self.conn = conn
      self.calfile = calfile
      self.status=rt_data.rt_status()
      self.protocol = DecadesProtocol
   
   def buildProtocol(self,addr):
      p = self.protocol(self.conn,self.status, self.calfile)
      p.factory = self
      return p


def main():# Listen for TCP:1500
   log.startLogging(stdout)
   conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")
   conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT)

   reactor.listenTCP(1500, DecadesFactory(conn,"pydecades/rt_calcs/HOR_CALIB.DAT"))
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported
