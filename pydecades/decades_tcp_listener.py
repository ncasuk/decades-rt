from twisted.internet.protocol import Protocol
from twisted.python import log
from datetime import datetime
from decades import DecadesDataProtocols
import os
from distutils.dir_util import mkpath

from ConfigParser import SafeConfigParser

class DecadesTCPListener(Protocol):
   dataProtocols = DecadesDataProtocols() 
   outfiles = {} #dictonary of output files.
   parser = SafeConfigParser()
   config = parser.read(['/etc/decades/decades.ini','decades.ini'])
   
   def __init__(self):
      self.output_dir = self.parser.get('Config','output_dir')
      #interprets the mode as an octal int
      self.output_create_mode = int(self.parser.get('Config','output_create_mode'),8)
      for each in self.dataProtocols.available():
         self.outfiles[each] = {} #dictionary per instrument for fligh #s
            
   def dataReceived(self, data):
      instrument=data[1:9] #e.g PRTAFT01
      flightno=data[20:24] # e.g. XXXX, SIMU, B751
      log.msg('TCP data from ' + instrument + ' ' + flightno)
      self.writedata(data, instrument, flightno)
   
   def writedata(self, data, instrument, flightno):
      try:
         self.outfiles[instrument][flightno].write(data)
      except KeyError: #i.e.file does not exist yet
         try: #try to create file 
            os.umask(0)
            dt = datetime.utcnow()
            outpath = os.path.join(self.output_dir,dt.strftime('%Y'), dt.strftime('%m'))
            mkpath(outpath, mode=self.output_create_mode + 0111) #acts like "mkdir -p" so if exists returns a success (+0111 adds executable bit as they ar dirs)

            outfile = os.path.join(outpath,instrument + '_'+dt.strftime('%Y%m%d_%H%M%S') +'_' + flightno)
            log.msg('Creating output file ' + outfile)
            self.outfiles[instrument][flightno] = os.fdopen(os.open(outfile, os.O_WRONLY | os.O_CREAT, self.output_create_mode), 'w')
         except TypeError: 
            '''usually some incoming data corruption so 'instrument' and/or 'flightno'
            are not valid due to containing some NULL bytes; ignore data in that case'''
            log.msg('Invalid TCP data, discarding')
            

