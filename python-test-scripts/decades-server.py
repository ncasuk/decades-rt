#!/usr/bin/python
from array import array

from twisted.internet import protocol, reactor
from twisted.application import internet, service
from twisted.protocols import basic
from twisted.python import log

from sys import stdout
import time
import math
from datetime import datetime, timedelta
import struct

#from decades_file import DecadesFile

#logfile
log.startLogging(file('/var/log/decades-server/' + 'decades_' + datetime.now().strftime('%Y-%m-%d_%H:%M:%S') + '.log', 'w'))

#class to handle Decades events
class DecadesProtocol(basic.LineReceiver):
   delimiter = "" #Java DatInputStream does not have a delimiter between lines
   derindex = 0
   der = []
   status_struct_fmt = ">bhh11f4c" # big-endian, byte, short, short, 11 floats, 4 characters
   #para_request_fmt = ">ii" # big-endian, int (starttime), int (endtime), plus some number of parameters
   def connectionMade(self):
      log.msg("connection from", self.transport.getPeer())
      self.setRawMode()

   def lineReceived(self, line):
      log.msg(line)

   def rawDataReceived(self, data):
      if data == "STAT":
         self.writeStatus()
      if data[0:4] == "PARA":
         formt = ">4s" + str((len(data)/4)-1) + 'i' # 4 characters (P A R A), start time integer, end time integer, integer indicating number of parameters, then integer codes for the parameters (see horaceplot/choices/PARANO.TXT)
         para = struct.unpack(formt,data)
         log.msg("Incoming: " +  repr(para))
         self.writeStatus()
         #log.msg(">iiffff",-1,para[3],self.time_seconds_past_midnight(),274,self.time_seconds_past_midnight(),274)
         self.der.append([self.time_seconds_past_midnight(),274.1 + (5 * math.sin(self.time_seconds_past_midnight()))])
         #self.sendLine(struct.pack(">ii",self.derindex,len(self.der[para[1]:])))
         self.sendLine(struct.pack(">ii",self.time_seconds_past_midnight(),len(self.der[para[1]:])))
         msg = [self.derindex, len(self.der[para[1]:])]
         for i in range(para[1], para[1] + len(self.der[para[1]:])):
            self.sendLine(struct.pack(">f",self.der[i][0]))
            msg.append(self.der[i][0])

         for i in range(para[1], para[1] + len(self.der[para[1]:])):
            self.sendLine(struct.pack(">f",self.der[i][1]))
            msg.append(self.der[i][1])

         log.msg(msg)
         self.derindex = self.derindex +1
         #log.msg('\x00@\x00\x00\x00@@\x00\x00')
         #self.sendLine('\x00@\x00\x00\x00@@\x00\x00')

   def writeStatus(self):
      #test status line
      #mapstatus (integer), derindex (integer), dercount (integer), t/s past 00:00 (float), GIN heading/degrees (float), static pressure millibars (float), Pressure height/feet (float), True air speed (float), True air temp de-iced (float), Dew point - General Eastern (float), Indicated wind speed (float), Indicated wind angle (float), Latitude from GIN (float), Longitude from GIN (float), flight number (4-character code, ASCII)
      #log.msg(self.status_struct_fmt,1,self.derindex,1,self.time_seconds_past_midnight(),1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,'T','E','S','T')
      #mapstatus (integer), derindex (integer), dercount (integer), t/s past 00:00 (float), Wind speed, ms-1, 
      self.sendLine(struct.pack(self.status_struct_fmt,1,self.derindex,len(self.der),self.time_seconds_past_midnight(),1.1,2.0,3.0,4.0,0.2,6.0,7.0,8.0,9.0,10.0,'T','E','S','T'))
      log.msg('STATus sent')
   
   def time_seconds_past_midnight(self):
      return time.time() - time.mktime(datetime.now().timetuple()[0:3]+(0,0,0,0,0,0))

class DecadesFactory(protocol.Factory):
   _recvd = {}
   protocol = DecadesProtocol

application = service.Application('decades')
reactor.listenTCP(1500, DecadesFactory())
reactor.run()
#factory = DecadesFactory()
#internet.TCPServer(1500, factory).setServiceParent(service.IServiceCollection(application))

