#!/usr/bin/env python
############################################################################

# Simple UDP Multicast Client for DECADES
# Dan Walker
# NCAS

from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor
from twisted.application.internet import MulticastServer
import psycopg2, csv
from decades import DecadesDataProtocols

conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")


class MulticastServerUDP(DatagramProtocol):
    dataProtocols = DecadesDataProtocols() 
    cursor = conn.cursor()
    def startProtocol(self):
        for proto in self.dataProtocols.available():
            print 'Creating table %s' % proto
            print(self.dataProtocols.create_table(proto, self.cursor))
        
        print 'Started Listening'
        # Join a specific multicast group, which is the IP we will respond to
        self.transport.joinGroup('225.0.0.0')
         

    def datagramReceived(self, datagram, address):
      #print repr(address) + ' : ' + repr(datagram)
      #data = datagram.split(',')
      data = csv.reader([datagram]).next()
      if data[0] == '$AERACK01':
         squirrel = 'INSERT INTO test_%s (%s)' % (self.dataProtocols.protocols[data[0][1:]][0]['field'].lstrip('$'), ', '.join(self.dataProtocols.fields(data[0][1:])))
         print len(data), len(self.dataProtocols.fields(data[0][1:]))
         processed= []
         for each in data:
            if each == '':
               processed.append(None)
            else:
               processed.append(each)
         print(self.cursor.execute(squirrel + ' VALUES (' + (','.join(['%s'] * len(data))) +')', processed))
         conn.commit();
   
      #print data[0]

# Note that the join function is picky about having a unique object
# on which to call join.  To avoid using startProtocol, the following is
# sufficient:
#reactor.listenMulticast(8005, MulticastServerUDP()).join('224.0.0.1')

# Listen for multicast on 224.0.0.1:8005
reactor.listenMulticast(50001, MulticastServerUDP())
reactor.run()
