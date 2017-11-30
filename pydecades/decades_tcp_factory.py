#!/usr/bin/python
from twisted.internet.protocol import Protocol, Factory
from twisted.internet import reactor
from twisted.python import log

from sys import stdout

from decades_tcp_listener import DecadesTCPListener
from decades import DecadesDataProtocols

class DecadesTCPFactory(Factory):
    protocol = DecadesTCPListener
    dataProtocols = DecadesDataProtocols() 
    outfiles = {} 

    def __init__(self):
      for each in self.dataProtocols.available():
         self.outfiles[each] = {} #dictionary per instrument for fligh #s

    def buildProtocol(self, addr):
      d = Factory.buildProtocol(self, addr)
      d.factory = self
      return d

def main():# Listen for TCP:3502
   log.startLogging(stdout)

   reactor.listenTCP(3502, DecadesTCPFactory())
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported
