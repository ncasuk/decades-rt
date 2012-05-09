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


class MulticastServerUDP(DatagramProtocol):
    dataProtocols = DecadesDataProtocols() 
    def __init__(self, conn):
        self.conn = conn
        self.cursor = self.conn.cursor()

    def startProtocol(self):
        for proto in self.dataProtocols.available():
            print 'Creating table %s' % proto
            print(self.dataProtocols.create_table(proto, self.cursor, '_' + self.dataProtocols.protocol_versions[proto]))
        
        print 'Started Listening'
        # Join a specific multicast group, which is the IP we will respond to
        self.transport.joinGroup('225.0.0.0')
         

    def datagramReceived(self, datagram, address):
      #print repr(address) + ' : ' + repr(datagram)
      #data = datagram.split(',')
      data = csv.reader([datagram]).next()
      squirrel = 'INSERT INTO %s_%s (%s)' % (self.dataProtocols.protocols[data[0][1:]][0]['field'].lstrip('$'), self.dataProtocols.protocol_versions[data[0][1:]], ', '.join(self.dataProtocols.fields(data[0][1:])))
      print len(data), len(self.dataProtocols.fields(data[0][1:]))
      processed= []
      for each in data:
         if each == '':
            processed.append(None)
         else:
            processed.append(each)
      self.cursor.execute(squirrel + ' VALUES (' + (','.join(['%s'] * len(data))) +')', processed)
      self.conn.commit();
  
      #print data[0]

# Note that the join function is picky about having a unique object
# on which to call join.  To avoid using startProtocol, the following is
# sufficient:
#reactor.listenMulticast(8005, MulticastServerUDP()).join('224.0.0.1')

def main():# Listen for multicast on 224.0.0.1:8005
   conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")

   reactor.listenMulticast(50001, MulticastServerUDP(conn))
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported

