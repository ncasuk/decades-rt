from twisted.internet.protocol import Protocol
from twisted.python import log
from datetime import datetime
from decades import DecadesDataProtocols as ddp

class DecadesTCPListener(Protocol):
   def dataReceived(self, data):
      log.msg('TCP data from ' + data[1:8])
      self.outfile = open('/opt/decades/output/decades-tcp-' + data[1:8] +'-'+datetime.utcnow().strftime('%Y-%m-%d_%H.%M.%S') +'.bin','w')
      self.outfile.write(data)
