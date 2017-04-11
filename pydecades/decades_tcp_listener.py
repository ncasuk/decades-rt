from twisted.internet.protocol import Protocol
from twisted.python import log
from datetime import datetime
import time
import os, struct, json, re
from distutils.dir_util import mkpath

from pydecades.configparser import DecadesConfigParser

class DecadesTCPListener(Protocol):
   #outfiles = {} #dictonary of output files.
   parser = DecadesConfigParser()
   __buffer = ''
   ''' Length, in bytes, of the required header of incoming TCP data'''
   header_length = 13
   INSTRUMENT = re.compile('\$[A-Z0-9]{8}') #e.g. "$CORCON01"
   
   def __init__(self):
      self.output_dir = self.parser.get('Config','output_dir')
      #interprets the mode as an octal int
      self.output_create_mode = int(self.parser.get('Config','output_create_mode'),8)
      log.msg('Initialising protocol')
            
   def dataReceived(self, data):
      '''reconstitutes a possibly-fragmented incoming TCP record'''

      self.__buffer = self.__buffer + data
      while len(self.__buffer) >= self.header_length:
          if self.INSTRUMENT.match(self.__buffer[0:9]):
              instrument=self.__buffer[0:9]
              if(instrument not in self.factory.dataformats):
                  self.factory.dataformats[instrument]=self.factory.defaultformat
                  self.factory.dataformats[instrument]['name']=instrument[1:]
              self.inst=self.factory.dataformats[instrument]
              self.header_length=self.inst['utc_time'][0]
              pl=self.inst['packet_length']
              tl=self.inst['totalbytes']
              (packet_length, ) =struct.unpack(pl[2],self.__buffer[pl[0]:pl[0]+pl[1]])
              if(packet_length!=tl-self.header_length):
                  log.msg('Length %i not what expected %i' % (packet_length,tl-self.header_length))
                  if(tl==0):
                      self.factory.dataformats[instrument]['totalbytes']=packet_length+self.header_length
                      
              #(packet_length, ) = struct.unpack('>I',self.__buffer[9:self.header_length])
              if len(self.__buffer)<self.header_length+packet_length:
                  #Incomplete; wait for more data
                  log.msg('Buffered %s bytes' % len(self.__buffer))
                  return
              else:
                  self.complete_record(self.__buffer[:self.header_length+packet_length])
                  self.__buffer=self.__buffer[self.header_length+packet_length:]                  
          else:    
              log.msg('Discarded %s bytes' % len(self.__buffer))
              #Drops TCP connection to console
              # so stream from it restarts "clean"
              log.msg(repr(self.__buffer))
              self.transport.loseConnection()
          
   #used to be the dataReceived method, above 
   def complete_record(self, data):
      '''gets the instrument name & flight number, and passes it to the write function'''
      if('flight_num' in self.inst):
          fn=self.inst['flight_num']
          (flightno,)=struct.unpack(fn[2],data[fn[0]:fn[0]+fn[1]])
      else:
          flightno='XXXX'
      #flightno=data[20:24] # e.g. XXXX, SIMU, B751
      log.msg('TCP data from ' + self.inst['name'] + ' ' + flightno)
      self.writedata(data, flightno)
   
   def writedata(self, data, flightno):
      utc=self.inst['utc_time']
      instrument=self.inst['name']
      if(data[utc[0]:utc[0]+4]==' NOW'): # Deal with cases which don't include time, but require it ...
                               # string ' NOW' would be 6/03/1987 long before any 146 flights
          data=data[:utc[0]]+struct.pack('>I',time.time())+data[utc[0]+4:]
          if(data[utc[0]+4:utc[0]+6]=='ms'): # Optional millisecond fraction
              data=data[:utc[0]+4]+struct.pack('>H',1000*(time.time() % 1))+data[utc[0]+6:]
      try:
         self.factory.outfiles[instrument][flightno].write(data)
         self.factory.outfiles[instrument][flightno].flush()
      except KeyError: #i.e.file does not exist yet
         try: #try to create file 
            os.umask(022)
            dt = datetime.utcnow()
            outpath = os.path.join(self.output_dir,dt.strftime('%Y'), dt.strftime('%m'), dt.strftime('%d'))
            mkpath(outpath, mode=self.output_create_mode + 0111) #acts like "mkdir -p" so if exists returns a success (+0111 adds executable bit as they are dirs)

            outfile = os.path.join(outpath,instrument + '_'+dt.strftime('%Y%m%d_%H%M%S') +'_' + flightno)
            log.msg('Creating output file ' + outfile)
            try:
               self.factory.outfiles[instrument][flightno] = open(outfile, 'w')
            except KeyError:
               #instrument hasn't a CSV file describing it for UDP
               self.factory.outfiles[instrument] = {} 
               self.factory.outfiles[instrument][flightno] = open(outfile, 'w')
            #write data
            self.factory.outfiles[instrument][flightno].write(data)
            self.factory.outfiles[instrument][flightno].flush()
            
 
         except TypeError: 
            '''usually some incoming data corruption so 'instrument' and/or 'flightno'
            are not valid due to containing some NULL bytes; ignore data in that case'''
            log.msg('Invalid TCP data, discarding')
      #update list-of-latest files
      latest_array = {} #Start with 'Missing' Entries
      for each in self.factory.outfiles:
         latest_array[each] = 'MISSING'

      try:
         current = open(os.path.join(self.output_dir,'latest'),'r')
         latest_array.update(json.load(current)) #file overrules MISSINGs
         current.close()
      except (IOError, ValueError): #file does not exist or is unreadable
         pass;
      with open(os.path.join(self.output_dir,'latest'),'w') as latest:#overwrites each time
         latest_array[instrument] = (self.factory.outfiles[instrument][flightno]).name
         #write JSON
         json.dump(latest_array,latest)

            

