from twisted.internet.protocol import Protocol
from twisted.python import log
from datetime import datetime
from decades import DecadesDataProtocols

class DecadesTCPListener(Protocol):
   dataProtocols = DecadesDataProtocols() 
   outfiles = {} #dictonary of output files.
   def __init__(self):
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
      except KeyError:
         self.outfiles[instrument][flightno] = open('/opt/decades/output/' + instrument +'_'+datetime.utcnow().strftime('%Y%m%d_%H%M%S') +'_' + flightno,'w')

