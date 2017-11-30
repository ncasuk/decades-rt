#!/usr/bin/python
from sys import stdout
from twisted.internet.protocol import Factory
from twisted.internet import reactor
from twisted.python import log
from twisted.protocols.portforward import ProxyServer, ProxyFactory

class Balancer(Factory):
    def __init__(self, hostports):
        self.factories = []
        for (host, port) in hostports:
            self.factories.append(ProxyFactory(host, port))
    def buildProtocol(self, addr):
        nextFactory = self.factories.pop(0)
        self.factories.append(nextFactory)
        return nextFactory.buildProtocol(addr)


def main():# Listen for TCP:1500
   log.startLogging(stdout)

   reactor.listenTCP(1500, Balancer([('127.0.0.1',1600), ('127.0.0.1',1601)]))
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported
