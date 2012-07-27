#!/usr/bin/python
from twisted.internet.protocol import Protocol, Factory
from twisted.internet import reactor
from decades_tcp_listener import DecadesTCPListener

class DecadesTCPFactory(Factory):
    def buildProtocol(self, addr):
        return DecadesTCPListener()

# 3502 is the port you want to run under. Choose something >1024
reactor.listenTCP(3502,DecadesTCPFactory())
reactor.run()
