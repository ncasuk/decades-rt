#!/usr/bin/python
from twisted.internet import reactor
from twisted.internet.protocol import Protocol, ReconnectingClientFactory, ServerFactory
from twisted.python import log
from datetime import datetime

class Proxy(Protocol):
   noisy = True
   peer = None
 
   def setPeer(self, peer):
      self.peer = peer
 
   def connectionLost(self, reason):
      if self.peer is not None:
         #self.peer.transport.loseConnection()
         self.peer.peer = None
      elif self.noisy:
         log.msg("Unable to connect to peer: %s" % (reason,))

#GIN server protocol
class GINServer(Proxy):
      
    def connectionMade(self):
       log.msg("Client connection made")
       self.peer.setPeer(self)
       #self.peer.transport.resumeProducing()

       
class GINServerFactory(ServerFactory):
   protocol = GINServer

   def setClient(self, client):
      self.client = client
   
   def buildProtocol(self, *args, **kw):
      log.msg("building server protocol")
      prot = ServerFactory.buildProtocol(self, *args, **kw)
      prot.setPeer(self.client)
      return prot

# GIN client protocol
class GINClient(Proxy):
    """Once connected, save incoming data"""

    peer = None
    serverFactory = GINServerFactory

    def __init__(self):
        self.outfile = open('/opt/decades/output/gindat_'+datetime.utcnow().strftime('%Y%m%d_%H%M%S')+'.bin','a')
    
    def connectionMade(self):
        log.msg( "Connected to GIN")
        server = self.serverFactory()
        server.setClient(self)
        reactor.listenTCP(self.factory.outPort, server, interface=self.factory.outAddress)
    
    def dataReceived(self, data):
        self.outfile.write(data)
        if(self.peer):
            self.peer.transport.write(data)


class GINClientFactory(ReconnectingClientFactory):
    protocol = GINClient
    serverprotocol = GINServer

    def __init__(self, outaddress="192.168.102.21", outport=5602):
        self.outAddress = outaddress
        self.outPort = outport

    def startedConnecting(self, connector):
        log.msg('Started to connect.')

    def buildProtocol(self, *args, **kw):
        log.msg( 'Resetting reconnection delay')
        prot = ReconnectingClientFactory.buildProtocol(self, *args, **kw)
        log.msg("Listening for clients on ",(self.outAddress, self.outPort))
        self.resetDelay()
        return prot

    def clientConnectionFailed(self, connector, reason):
        log.msg('Connection failed. Reason:', reason)
        ReconnectingClientFactory.clientConnectionFailed(self, connector, reason)
    
    def clientConnectionLost(self, connector, reason):
        log.msg('Lost connection.  Reason:', reason)
        ReconnectingClientFactory.clientConnectionLost(self, connector, reason)


# this connects the protocol to the GIN and starts rebroadcasting
def main():
    f = GINClientFactory()
    reactor.connectTCP("192.168.101.21", 5602, f)
    reactor.run()

# this only runs if the module was *not* imported
if __name__ == '__main__':
    main()
