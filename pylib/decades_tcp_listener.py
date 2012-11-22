from twisted.internet.protocol import Protocol
from twisted.python import log
from datetime import datetime
from decades import DecadesDataProtocols

class DecadesTCPListener(Protocol):
   dataProtocols = DecadesDataProtocols() 
   outfiles = []
   def __init__(self):
      for each in self.dataProtocols.available():
         self.outfiles[each] = open('/opt/decades/output/decades-tcp-' + each +'-'+datetime.utcnow().strftime('%Y-%m-%d_%H.%M.%S') +'.bin','w')
            
   def dataReceived(self, data):
      log.msg('TCP data from ' + data[1:9])
      self.outfiles[data[1:9]].write(data)
