#!/usr/bin/python
'''Listens for requests from the Java client applet and responds appropriately'''
from array import array

from twisted.internet import protocol, reactor
from twisted.application import internet, service
from twisted.protocols import basic
from twisted.python import log

import psycopg2 

from sys import stdout
import time
import math
from datetime import datetime, timedelta
import struct

conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata_test")

#from decades_file import DecadesFile

#logfile
log.startLogging(file('/var/log/decades-server/' + 'decades_' + datetime.now().strftime('%Y-%m-%d_%H:%M:%S') + '.log', 'w'))

#class to handle Decades events
class DecadesProtocol(basic.LineReceiver):
   '''Python version of the HORACE server - to work with DECADES'''
   delimiter = "" #Java DatInputStream does not have a delimiter between lines
   derindex = 0
   der = []
   cursor = conn.cursor()
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
         #self.der.append([self.time_seconds_past_midnight(),274.1 + (5 * math.sin(self.time_seconds_past_midnight()))])
         #send integer: current max time-index
         self.sendLine(struct.pack(">i",self.derindex))
         #old pre DB data
         '''msg = [self.derindex, len(self.der[para[1]:])]
         for i in range(para[1], para[1] + len(self.der[para[1]:])):
            self.sendLine(struct.pack(">f",self.der[i][0]))
            msg.append(self.der[i][0])

         for i in range(para[1], para[1] + len(self.der[para[1]:])):
            self.sendLine(struct.pack(">f",self.der[i][1]))
            msg.append(self.der[i][1])'''

         #look-up table
         parano = {515: "time_from_midnight", 520: "deiced_true_airtemp"}
       
         paralist = []
         for paracode in para[4:]:
            paralist.append(parano[paracode])

         if len(paralist) == 0:
            paralist.append('id') #so that the no-data request returns correctly

         if para[2] == -1:
            #it wants all up to latest data point
            self.cursor.execute('SELECT ' + ', '.join(paralist) + ' FROM scratchdata WHERE id > %s ',(para[1],))
         else:
            #in this case there is a specific range it wants
            self.cursor.execute('SELECT ' + ', '.join(paralist) + ' FROM scratchdata WHERE id BETWEEN %s AND %s',(para[1],para[2]))
        
         log.msg(self.cursor.query) 
         returndata = self.cursor.fetchall()
         #send integer of size of upcoming data
         self.sendLine(struct.pack(">i",len(returndata)))
         log.msg(repr(len(returndata)))
         

         log.msg('requesting data between %i and %i, returning %i datapoints' % (para[1],para[2],len(returndata)) )
         #send each requested parameter separately
         for index in range(len(para[4:])):
            for tup in returndata: 
               log.msg(repr(returndata.index(tup)))
               self.sendLine(struct.pack(">f",tup[index]))
            

         #log.msg(msg)
         #self.derindex = self.derindex +1
         #log.msg('\x00@\x00\x00\x00@@\x00\x00')
         #self.sendLine('\x00@\x00\x00\x00@@\x00\x00')

   def writeStatus(self):
      #test status line
      #mapstatus (integer), derindex (integer), dercount (integer), t/s past 00:00 (float), GIN heading/degrees (float), static pressure millibars (float), Pressure height/feet (float), True air speed (float), True air temp de-iced (float), Dew point - General Eastern (float), Indicated wind speed (float), Indicated wind angle (float), Latitude from GIN (float), Longitude from GIN (float), flight number (4-character code, ASCII)
      #log.msg(self.status_struct_fmt,1,self.derindex,1,self.time_seconds_past_midnight(),1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,'T','E','S','T')
      #mapstatus (integer), derindex (integer), dercount (integer), t/s past 00:00 (float), Wind speed, ms-1, 
      #log.msg(repr(self.der))
      self.cursor.execute("SELECT id, id FROM scratchdata ORDER BY id DESC LIMIT 1;")
      (self.derindex, dercount) = self.cursor.fetchone()
      self.sendLine(struct.pack(self.status_struct_fmt,1,self.derindex,dercount,self.time_seconds_past_midnight(),1.1,2.0,3.0,4.0,0.2,6.0,7.0,8.0,9.0,10.0,'T','E','S','T'))
      log.msg('STATus sent (derindex, dercount)' + str((self.derindex, dercount)))
   
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

