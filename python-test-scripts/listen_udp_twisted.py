#!/usr/bin/env python
############################################################################

# Simple UDP Multicast Client for DECADES
# Dan Walker`
# NCAS

from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor
from twisted.application.internet import MulticastServer
import psycopg2, csv
import protocol-class

conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")


class MulticastServerUDP(DatagramProtocol):
    dataProtocols = DecadesDataProtocols() 
    cursor = conn.cursor()
    def startProtocol(self):
        print 'Creating tables'
        for proto in dataProtocols.available():
            dataProtocols.create_table(proto,
        
        print 'Started Listening'
        # Join a specific multicast group, which is the IP we will respond to
        self.transport.joinGroup('225.0.0.0')
         

    def datagramReceived(self, datagram, address):
      #print repr(address) + ' : ' + repr(datagram)
      #data = datagram.split(',')
      data = csv.reader([datagram]).next()
      if data[0] == '$AERACK01':
         print(repr(data))
         
      conn.commit();
   
      #print data[0]

# Note that the join function is picky about having a unique object
# on which to call join.  To avoid using startProtocol, the following is
# sufficient:
#reactor.listenMulticast(8005, MulticastServerUDP()).join('224.0.0.1')

# Listen for multicast on 224.0.0.1:8005
reactor.listenMulticast(50001, MulticastServerUDP())
reactor.run()
