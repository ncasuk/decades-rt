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
from rt_calcs import rt_derive
from ConfigParser import SafeConfigParser

#from decades_file import DecadesFile
#logfile
#log.startLogging(file('/var/log/decades-server/' + 'decades_' + datetime.now().strftime('%Y-%m-%d_%H:%M:%S') + '.log', 'w'))

#class to handle Decades events
class DecadesProtocol(basic.LineReceiver):
   '''Python version of the HORACE server - to work with DECADES'''
   delimiter = "" #Java DatInputStream does not have a delimiter between lines
   derindex = 0
   der = []
   status_struct_fmt = ">bhh11f4c" # big-endian, byte, short, short, 11 floats, 4 characters
   #para_request_fmt = ">ii" # big-endian, int (starttime), int (endtime), plus some number of parameters
   def __init__(self, conn, calfile="pylib/rt_calcs/HOR_CALIB.DAT"):
       '''Takes a database connection, and creates a NamedTuple cursor (allowing us to
         access the results by fieldname *or* index'''
       self.cursor = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
       self.rtlib = rt_derive.derived(self.cursor,calfile) #class processing the cals & producing "real" values
       self.parano = {}
       parser = SafeConfigParser()
       self.config = parser.read('pylib/decades.ini')
       for (code, function) in parser.items('Parameters'):
         self.parano[int(code)] = function
         
      
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
         #log.msg("Incoming: " +  repr(para))
         self.writeStatus()
         #log.msg(">iiffff",-1,para[3],self.time_seconds_past_midnight(),274,self.time_seconds_past_midnight(),274)
         #self.der.append([self.time_seconds_past_midnight(),274.1 + (5 * math.sin(self.time_seconds_past_midnight()))])
         #send integer: current max time-index
         self.sendLine(struct.pack(">i",self.derindex))
         
       
         paralist = []
         for paracode in para[4:]:
            paralist.append(self.parano[paracode])

         if len(paralist) == 0:
            paralist.append('id') #so that the no-data request returns correctly

         if para[2] == -1:
            #it wants all up to latest data point
            returndata = self.rtlib.derive_data_alt(paralist, '> %i' % para[1])
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
         for paracode in para[4:]: #list of required fields
               sendable = returndata[self.parano[paracode]]
               for each in sendable:
                  self.sendLine(struct.pack(">f",each))
            

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
      statusdata = self.rtlib.derive_data_alt(['derindex','gin_heading','static_pressure','pressure_height_feet','true_air_speed', 'deiced_true_air_temp_c','dew_point','gin_wind_speed','wind_angle','gin_latitude','gin_longitude'], '=id','ORDER BY id DESC LIMIT 1')
      #self.cursor.execute("SELECT id, id AS dercount, GREATEST(gindat01_heading_gin,-1.0) AS gindat01_heading_gin FROM mergeddata WHERE uppbbr01_utc_time IS NOT NULL AND corcon01_utc_time IS NOT NULL AND aerack01_utc_time IS NOT NULL AND lowbbr01_utc_time IS NOT NULL ORDER BY id DESC LIMIT 1;")
      #(self.derindex, dercount, gindat01_heading_gin) = self.cursor.fetchone()
      (self.derindex, dercount) = (statusdata['derindex'], statusdata['derindex'])
      self.sendLine(struct.pack(self.status_struct_fmt,1,self.derindex,dercount,self.time_seconds_past_midnight(),statusdata['gin_heading'],statusdata['static_pressure'],statusdata['pressure_height_feet'],statusdata['true_air_speed'],statusdata['deiced_true_air_temp_c'],statusdata['dew_point'],statusdata['gin_wind_speed'],statusdata['wind_angle'],statusdata['gin_latitude'],statusdata['gin_longitude'],'T','E','S','T'))
      #log.msg('STATus sent (derindex, dercount)' + str((self.derindex, dercount)))
   
   def time_seconds_past_midnight(self):
      return time.time() - time.mktime(datetime.now().timetuple()[0:3]+(0,0,0,0,0,0))

class DecadesFactory(protocol.ServerFactory):
   _recvd = {}
   def __init__(self, conn, calfile):
      self.conn = conn
      self.calfile = calfile
      self.protocol = DecadesProtocol
   
   def buildProtocol(self,addr):
      p = self.protocol(self.conn, self.calfile)
      p.factory = self
      return p


def main():# Listen for TCP:1500
   log.startLogging(stdout)

   conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")
   conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT)

   reactor.listenTCP(1500, DecadesFactory(conn,"pylib/rt_calcs/HOR_CALIB.DAT"))
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported
