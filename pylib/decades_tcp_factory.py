#!/usr/bin/python
from twisted.internet.protocol import Protocol, Factory
from twisted.internet import reactor
from twisted.python import log

from sys import stdout

from decades_tcp_listener import DecadesTCPListener

class DecadesTCPFactory(Factory):
    def buildProtocol(self, addr):
        return DecadesTCPListener()

def main():# Listen for TCP:3502
   log.startLogging(stdout)

   reactor.listenTCP(3502, DecadesTCPFactory())
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported
