#!/usr/bin/env python
############################################################################


from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor, error
from twisted.internet.defer import Deferred 
import psycopg2, csv, psycopg2.extensions, time, datetime, _csv,psycopg2.extras
from decades import DecadesDataProtocols
from rt_calcs import rt_derive,rt_data
from twisted.python import log 

#from retryingcall import RetryingCall, simpleBackoffIterator
from pydecades.configparser import DecadesConfigParser
from pydecades.database import get_database
from collections import namedtuple # so we can used namedtuple cursors
from distutils.dir_util import mkpath
   
import os

class SeaUDP(DatagramProtocol):
    '''Simple UDP Client for SEA probe DECADES

    * Dave Tiddeman, Met Office
    * Dan Walker, NCAS

    Listens on UDP port 2100 for data from the SEA probe. Writes incoming data 
    packets (prefixed ``d[1-3]``) and calibration packets (``c0``) to an outfile
    in the DECADES data dir.

    Additionally, uses standard :func:`~pydecades.rt_calcs.rt_data.rt_data.derive_data_alt` 
    to get the static_pressure (hPa), deiced true air temp (C), and TAS (m/s) to 
    feed to UDP port 2110 on the SEA probe. This will trigger real data values which
    can be displayed on the live system.'''
    dataProtocols = DecadesDataProtocols()
    parser = DecadesConfigParser()
    last_time=0
    instname='seaprobe'
    outfiles={}
    maxTimeError=3
    def __init__(self):
        '''Takes a database connection, and creates a cursor'''
        self.conn =  get_database()
        self.cursor = self.conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
        self.output_dir = self.parser.get('Config','output_dir')
        #interprets the mode as an octal int
        self.calfile = self.parser.get('Config','calfile')
        self.rtlib = rt_derive.derived(self.cursor, self.calfile)
        self.output_create_mode = int(self.parser.get('Config','output_create_mode'),8)
        self.conditions = '=id'
        self.orderby = ' ORDER BY utc_time DESC LIMIT 1'
        self.readparas=["flight_number","static_pressure","deiced_true_air_temp_c","true_air_speed_ms"]
        #self.writeparas={"sea_twc_v":2,"sea_twc_i":3,"sea_twc_t":4,"sea_083_v":6,"sea_083_i":7,"sea_083_t":8,
        #                 "sea_021_v":10,"sea_021_i":11,"sea_021_t":12,"sea_cmp_v":14,"sea_cmp_i":15,"sea_cmp_t":16 }  # Raw parameters
        self.writeparas={"sea_twc":2,"sea_lwc083":4,"sea_lwc021":6 }

        #details of UDP multicast group for sending SEA output in UDP packet
        self.host = "239.1.4.6"
        self.port = 50001



    def startProtocol(self):
        '''run on listener start'''
        log.msg('Started Listening to SEA probe')
         
    def stopProtocol(self):
        '''Run on listener stop'''
        self.outfiles[self.flightno].close() #just being tidy :)
        log.msg('Stopped Listening to SEA probe')
         
    def datagramReceived(self, datagram, address):
      '''reads an incoming UDP datagram, splits it up, INSERTs into database'''
      flight_data=self.rtlib.derive_data_alt(self.readparas,self.conditions,self.orderby)
      self.flightno = flight_data['flight_number'][0]
      try:
         data = csv.reader([datagram.replace('\x00','')]).next() #assumes only one record, strips NULL
         #log.msg(data[0])
         if not(data[0][0] =="d" or data[0][0] =="c"):
             raise _csv.Error("Datagram doesn't start 'd*'")
         t=time.time()
         if(data[0]=="d3"):
             sea_datetime=datetime.datetime.strptime(data[1]+"T"+data[2]+"000","%Y/%m/%dT%H:%M:%S.%f")
             sea_time=time.mktime(sea_datetime.timetuple()) + sea_datetime.microsecond * 0.000001
             if(abs(sea_time-t)>self.maxTimeError):
                 # datagram[3:26]=time.strftime(t,"%Y/%m/%d,%H:%M:%S",time.gmtime(t))+".{}".format(int((t % 1)*1000)) #replace time ?
                 pass #  What do you want to do if sea probe time is wrong ?
             #sends pressure, temp, TAS back to SEAPROBE
             self.send_airdata(address,flight_data)
         elif(data[0]=="d1"):
             #copies data into a dictionary
             paras=self.writeparas.copy()
             for p in paras:
                 paras[p]=data[self.writeparas[p]]
             paras["utc_time"]=int(t)
             #log.msg('writing to DB')
             self.dataProtocols.add_data(self.cursor, paras,('%s' % (self.instname, )).lower())
             #log.msg('DB write done')
         self.writedata(datagram) # write data to file
      except _csv.Error:
         log.msg('CSV failed to unpack, type='+data[0])
  
    def writedata(self, data):
      '''Writes data to outputfile'''
      try:
         self.outfiles[self.flightno].write(data)
         self.outfiles[self.flightno].flush()
         #log.msg('Writing to output file ')
      except KeyError: #i.e.file does not exist yet
         try: #try to create file 
            os.umask(022)
            dt = datetime.datetime.utcnow()
            outpath = os.path.join(self.output_dir,dt.strftime('%Y'), dt.strftime('%m'), dt.strftime('%d'))
            mkpath(outpath, mode=self.output_create_mode + 0111) #acts like "mkdir -p" so if exists returns a success (+0111 adds executable bit as they are dirs)

            outfile = os.path.join(outpath,self.instname + '_'+dt.strftime('%Y%m%d_%H%M%S') +'_' + self.flightno)
            log.msg('Creating output file ' + outfile)
            try:
               self.outfiles[self.flightno] = open(outfile, 'w')
            except KeyError:
               #instrument hasn't a CSV file describing it for UDP
               self.outfiles = {} 
               self.outfiles[self.flightno] = open(outfile, 'w')
            #write data
            self.outfiles[self.flightno].write(repr(data))
            self.outfiles[self.flightno].flush()
            
 
         except TypeError: 
            '''usually some incoming data corruption so 'instrument' and/or 'flightno'
            are not valid due to containing some NULL bytes; ignore data in that case'''
            log.msg('Invalid SEA UDP data, discarding')


    def send_airdata(self,(source_host, source_port),flight_data):
        UDP_out="{} {} {} 0\n".format(flight_data["static_pressure"][0],flight_data["deiced_true_air_temp_c"][0],flight_data["true_air_speed_ms"][0])
        #UDP_out="{} {} {} 0\n".format(flight_data["static_pressure"][0],17.4,20.1)
        #log.msg(source_host, UDP_out)
        self.transport.write(UDP_out, (source_host, 2110))

def main():# Listen for UDP on 2100
   '''This function is only called if this file is executed directly
      rather than via twistd and the TAC control file'''
   import sys                     #but it needs to be started here.
   log.startLogging(sys.stdout)
   reactor.listenUDP(2100, SeaUDP())
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported

