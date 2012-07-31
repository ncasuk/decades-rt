from twisted.internet.protocol import Protocol
from datetime import datetime

class DecadesTCPListener(Protocol):
   def __init__(self):
      self.outfile = open('/opt/deades/output/decades-tcp-' + datetime.utcnow().strftime('%Y-%m-%d_%H.%M.%S') +'.bin','w')

   def dataReceived(self, data):
      self.outfile.write(data)
