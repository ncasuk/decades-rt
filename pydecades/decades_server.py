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
from pydecades.configparser import DecadesConfigParser

#class to handle Decades events
class DecadesProtocol(basic.LineReceiver):
   '''Python version of the HORACE server - to work with DECADES system. Responds to two commands:
         STAT (returns basic status data e.g. lat/long, heading etc.)
         PARA (Returns requested parameters)'''
   delimiter = "" #Java DatInputStream does not have a delimiter between lines
   derindex = 0
   stat_output_format = "{0:.2f}"   #Format string for those output variables that are displayed unmodified in STAT lines 2 d.p at present
   der = []
   status_struct_fmt = ">bhh11f4c" # big-endian, byte, short, short, 11 floats, 4 characters
   #para_request_fmt = ">ii" # big-endian, int (starttime), int (endtime), plus some number of parameters
   def __init__(self, conn, calfile="pydecades/rt_calcs/HOR_CALIB.DAT"):
       '''Takes a database connection, and creates a NamedTuple cursor (allowing us to
         access the results by fieldname *or* index'''
       self.cursor = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
       self.rtlib = rt_derive.derived(self.cursor,calfile) #class processing the cals & producing "real" values
       self.parser = DecadesConfigParser()
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
         #self.der.append([self.time_seconds_past_midnight(),274.1 + (5 * math.sin(self.time_seconds_past_midnight()))])
         #send integer: current max time-index
         self.sendLine(struct.pack(">i",self.derindex))
         
       
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
      #test status line
      #mapstatus (integer), derindex (integer), dercount (integer), t/s past 00:00 (float), GIN heading/degrees (float), static pressure millibars (float), Pressure height/feet (float), True air speed (float), True air temp de-iced (float), Dew point - General Eastern (float), Indicated wind speed (float), Indicated wind angle (float), Latitude from GIN (float), Longitude from GIN (float), flight number (4-character code, ASCII)
      #log.msg(self.status_struct_fmt,1,self.derindex,1,self.time_seconds_past_midnight(),1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,'T','E','S','T')
      #log.msg(repr(self.der))
      prtgindata = self.rtlib.derive_data_alt(['time_since_midnight','derindex','flight_number','pressure_height_kft','static_pressure','gin_latitude','gin_longitude','gin_heading'], '=id','ORDER BY id DESC LIMIT 1')
      #get corcon separately so gin/prt stuff is independant of it.
      corcondata = self.rtlib.derive_data_alt(['time_since_midnight','derindex','true_air_speed', 'deiced_true_air_temp_c','dew_point','gin_wind_speed','wind_angle'], '=id','ORDER BY id DESC LIMIT 1')
      #(self.derindex, dercount, gindat01_heading_gin) = self.cursor.fetchone()
      if(corcondata['time_since_midnight'] and abs(corcondata['derindex'] - prtgindata['derindex']) < 10):
         (self.derindex, dercount) = (prtgindata['derindex'], prtgindata['derindex'])
         outline = (struct.pack(self.status_struct_fmt,
		      1,
		      self.derindex % 32768,
		      dercount if(dercount < 32768) else 32767,
		      prtgindata['time_since_midnight'],
		      prtgindata['gin_heading'],
		      prtgindata['static_pressure'],
		      prtgindata['pressure_height_kft'],
		      corcondata['true_air_speed'],
		      float(self.stat_output_format.format(corcondata['deiced_true_air_temp_c'][0])),
		      float(self.stat_output_format.format(corcondata['dew_point'][0])),
		      corcondata['gin_wind_speed'],
		      corcondata['wind_angle'],
		      prtgindata['gin_latitude'],
		      prtgindata['gin_longitude'],
		      prtgindata['flight_number'][0][0],
		      prtgindata['flight_number'][0][1],
		      prtgindata['flight_number'][0][2],
		      prtgindata['flight_number'][0][3]
         ))
      elif (prtgindata['time_since_midnight']):
         '''This is probably a data shortage, so only return minimal PRTAFT/GINDAT-only stuff'''
         log.msg('Data shortage, retrying with PRTAFT/GINDAT-only')
         (self.derindex, dercount) = (prtgindata['derindex'], prtgindata['derindex'])
         outline = (struct.pack(self.status_struct_fmt,
		      1,
		      self.derindex % 32768,
		      dercount if(dercount < 32768) else 32767,
		      prtgindata['time_since_midnight'],
		      prtgindata['gin_heading'],
		      prtgindata['static_pressure'],
		      prtgindata['pressure_height_kft'],
            float('NaN'),
            float('NaN'),
            float('NaN'),
            float('NaN'),
            float('NaN'),
		      prtgindata['gin_latitude'],
		      prtgindata['gin_longitude'],
		      prtgindata['flight_number'][0][0],
		      prtgindata['flight_number'][0][1],
		      prtgindata['flight_number'][0][2],
		      prtgindata['flight_number'][0][3]
         ))
      else:
         log.msg('No data, send null response')
         outline = (struct.pack(self.status_struct_fmt,
		      1,
		      self.derindex % 32768,
		      self.derindex if(self.derindex < 32768) else 32767,
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      float('NaN'),
		      '#',
		      '#',
		      '#',
		      '#',
         ))
         
      self.sendLine(outline)    
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

   reactor.listenTCP(1500, DecadesFactory(conn,"pydecades/rt_calcs/HOR_CALIB.DAT"))
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported
