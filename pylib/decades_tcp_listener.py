from twisted.internet.protocol import Protocol

class DecadesTCPListener(Protocol):
   def __init__(self):
      self.outfile = open('3502.out','w')

   def dataReceived(self, data):
      self.outfile.write(data)
