#!/usr/bin/python
from twisted.internet import reactor
from twisted.internet.protocol import Protocol, ReconnectingClientFactory, ServerFactory
from twisted.python import log
from datetime import datetime
import os
import json
from distutils.dir_util import mkpath

from pydecades.configparser import DecadesConfigParser

class Proxy(Protocol):
   '''GIN recorder and forwarder for DECADES
   
   * Dan Walker, NCAS
   
   Listens on address/port specified in ``decades.ini`` for incoming GIN data. It then rebroadcasts the data on outport/outaddress,
   also as specified, and also records the incoming data in the output dir.'''
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
    parser = DecadesConfigParser()

    def __init__(self):
        self.output_dir = self.parser.get('Config','output_dir')
        #interprets the mode as an octal int
        self.output_create_mode = int(self.parser.get('Config','output_create_mode'),8)
        os.umask(022)
        dt = datetime.utcnow()
        outpath = os.path.join(self.output_dir,dt.strftime('%Y'), dt.strftime('%m'), dt.strftime('%d'))
        mkpath(outpath, mode=self.output_create_mode + 0111) #acts like "mkdir -p" so if exists returns a success (+0111 adds executable bit as they are dirs)
        
        self.outfile = open(os.path.join(outpath,'GINDAT01_' + datetime.utcnow().strftime('%Y%m%d_%H%M%S')+'.bin'),'a')
    
    def connectionMade(self):
        log.msg( "Connected to GIN")
        server = self.serverFactory()
        server.setClient(self)
        reactor.listenTCP(self.factory.outPort, server, interface=self.factory.outAddress)
    
    def dataReceived(self, data):
        self.outfile.write(data)
        self.outfile.flush()
        #try to update 'latest' file for status page
        try:
           current = open(os.path.join(self.output_dir,'latest'),'r')
           latest_array = json.load(current)
           current.close()
        except (IOError, ValueError): #file does not exist or is unreadable
           latest_array = {} #empty Dict 
        with open(os.path.join(self.output_dir,'latest'),'w') as latest:#overwrites each time
           latest_array['GINDAT01'] = self.outfile.name
           #write JSON
           json.dump(latest_array,latest)

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
