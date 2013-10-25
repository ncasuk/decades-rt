from twisted.internet.protocol import Protocol
from twisted.python import log
from datetime import datetime
import os, struct
from distutils.dir_util import mkpath

from pydecades.configparser import DecadesConfigParser

class DecadesTCPListener(Protocol):
   #outfiles = {} #dictonary of output files.
   parser = DecadesConfigParser()
   __buffer = ''
   ''' Length, in bytes, of the required header of incoming TCP data'''
   header_length = 13
   
   def __init__(self):
      self.output_dir = self.parser.get('Config','output_dir')
      #interprets the mode as an octal int
      self.output_create_mode = int(self.parser.get('Config','output_create_mode'),8)
      log.msg('Initialising protocol')
            
   def dataReceived(self, data):
      '''reconstitutes a possibly-fragmented incoming TCP record'''

      self.__buffer = self.__buffer + data
      if len(self.__buffer) <= self.header_length:
         #incomplete, and still in the header; wait for more data
         return
      
      #Decode packet length
      (packet_length, ) = struct.unpack('>I',self.__buffer[9:self.header_length])
      log.msg('%s %s arrived, %s is full length' % (self.__buffer[1:9], len(data), packet_length+self.header_length))
      #split buffer into expected size chunks
      chunks = [self.__buffer[i:i+packet_length+self.header_length] for i in range(0, len(self.__buffer), packet_length+self.header_length)]
      while chunks:
         line = chunks.pop()
         if(len(line) == packet_length+self.header_length):
            self.complete_record(line)
            #strip that line from the buffer
            self.__buffer = "".join(chunks)
         elif (len(line) < packet_length+self.header_length):
            #it's trailing incomplete data
            log.msg('Buffered %s bytes' % len(line))
            self.__buffer = line
         else:
            #should never happen...
            raise ValueError
 
   #used to be the dataReceived method, above 
   def complete_record(self, data):
      '''gets the instrument name & flight number, and passes it to the write function'''
      instrument=data[1:9] #e.g PRTAFT01
      flightno=data[20:24] # e.g. XXXX, SIMU, B751
      log.msg('TCP data from ' + instrument + ' ' + flightno)
      self.writedata(data, instrument, flightno)
   
   def writedata(self, data, instrument, flightno):
      try:
         self.factory.outfiles[instrument][flightno].write(data)
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
            #write data
            self.factory.outfiles[instrument][flightno].write(data)
 
         except TypeError: 
            '''usually some incoming data corruption so 'instrument' and/or 'flightno'
            are not valid due to containing some NULL bytes; ignore data in that case'''
            log.msg('Invalid TCP data, discarding')
      #update list-of-latest files
      with open(os.path.join(self.output_dir,'latest'),'w') as latest:#overwrites each time
         #latest.write(instrument+': '+ outfile)
         for each in self.factory.outfiles:
            if flightno in self.factory.outfiles[each]:
               latest.write(each+': ' + ((self.factory.outfiles[each][flightno]).name) +'\r\n')
            else:
               latest.write(each+': ' + 'MISSING' +'\r\n')
            

