#!/usr/bin/env python
############################################################################

# Simple UDP Multicast Client for DECADES
# Dan Walker
# NCAS

from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor, error
from twisted.internet.defer import Deferred 
import psycopg2, csv, psycopg2.extensions, time
from decades import DecadesDataProtocols
from twisted.python import log 
    


def joinGroupErrorHandler(failure):
   #detect and handle MulticastJoinErrors
   failure.trap(error.MulticastJoinError)
   log.msg("MulticastJoinError - retrying")
   time.sleep(5)

def joinGroupSuccessHandler(result):
   log.msg("MulticastJoin - Success")

class MulticastServerUDP(DatagramProtocol):
    dataProtocols = DecadesDataProtocols() 
    def __init__(self, conn):
        '''Takes a database connection, and creates a cursor'''
        self.conn = conn
        self.cursor = self.conn.cursor()

    def startProtocol(self):
        '''Creates tables as required, starts the listener'''
        for proto in self.dataProtocols.available():
            self.dataProtocols.create_table(proto, self.cursor, '_' + self.dataProtocols.protocol_versions[proto])
        log.msg('CREATE VIEW')
        self.dataProtocols.create_view(self.cursor)
      
        is_fail = True 
        while is_fail: 
            is_fail = False
            # Join a specific multicast group, which is the IP we will respond to
            d = self.transport.joinGroup('225.0.0.0') #d is Deferred object
            d.addCallbacks(joinGroupSuccessHandler, joinGroupErrorHandler) #detect & handle join errors
   
        log.msg('Started Listening')
         
    def datagramReceived(self, datagram, address):
      '''reads an incoming UDP datagram, splits it up, INSERTs into database'''
      data = csv.reader([datagram]).next() #assumes only one record
      #copies data into a dictionary
      dictdata = dict(zip(self.dataProtocols.fields(data[0][1:]), data)) 
      print dictdata
      self.dataProtocols.add_data(self.cursor, dictdata,('%s' % (self.dataProtocols.protocols[data[0][1:]][0]['field'].lstrip('$'), )).lower())
      squirrel = 'INSERT INTO %s_%s (%s)' % (self.dataProtocols.protocols[data[0][1:]][0]['field'].lstrip('$'), self.dataProtocols.protocol_versions[data[0][1:]], ', '.join(self.dataProtocols.fields(data[0][1:])))
      processed= []
      for each in data:
         if each == '':
            processed.append(None)
         else:
            processed.append(each)
      if len(data)==len(self.dataProtocols.fields(data[0][1:])):
         self.cursor.execute(squirrel + ' VALUES (' + (','.join(['%s'] * len(data))) +')', processed)
         log.msg("Insert into %s successful" % data[0][1:])
      else:
         log.err("ERROR: Insert into %s failed, mismatched number of fields (%i, expecting %i)" % (data[0][1:], len(data), len(self.dataProtocols.fields(data[0][1:]))))
      #self.conn.commit();
  

# Note that the join function is picky about having a unique object
# on which to call join.  To avoid using startProtocol, the following is
# sufficient:
#reactor.listenMulticast(8005, MulticastServerUDP()).join('224.0.0.1')

def main():# Listen for multicast on 224.0.0.1:8005
   '''This function is only called if this file is executed directly
      rather than via twistd and the TAC control file'''
   import sys                     #but it needs to be started here.
   log.startLogging(sys.stdout)
   conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata"
                           )
   conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT)
   reactor.listenMulticast(50001, MulticastServerUDP(conn))
   reactor.run()

if __name__ == '__main__':
    main() #run if this file is called directly, but not if imported

