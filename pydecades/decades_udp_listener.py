#!/usr/bin/env python
############################################################################

# Simple UDP Multicast Client for DECADES
# Dan Walker
# NCAS

from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor, error
from twisted.internet.defer import Deferred 
import psycopg2, csv, psycopg2.extensions, time, _csv
from decades import DecadesDataProtocols
from twisted.python import log 

from retryingcall import RetryingCall, simpleBackoffIterator
    


class multicastJoinFailureTester(object):
   okErrs=(error.MulticastJoinError)

   def __call__(self, failure):
      failure.trap(self.okErrs)

class MulticastServerUDP(DatagramProtocol):
    dataProtocols = DecadesDataProtocols()
    maxTimeError=300 
    def __init__(self, conn):
        '''Takes a database connection, and creates a cursor'''
        self.conn = conn
        self.cursor = self.conn.cursor()

    def startProtocol(self):
        '''Creates tables as required, starts the listener'''
        for proto in self.dataProtocols.available():
            self.dataProtocols.create_table(proto, self.cursor, '_' + self.dataProtocols.protocol_versions[proto])
   
        if self.dataProtocols.new_table_count > 0:
            #one of the dataformat files has been updated, recreate merge table
            log.msg('Recreating mergeddata table')
            self.dataProtocols.create_maintable(self.cursor)
        else:
            self.dataProtocols.reuse_table(self.cursor)
      
        # Join a specific multicast group, which is the IP we will respond to
        r = RetryingCall(self.transport.joinGroup, '239.1.4.6')
        d = r.start(backoffIterator=simpleBackoffIterator(maxResults=20), failureTester=multicastJoinFailureTester())
   
        log.msg('Started Listening')
         
    def datagramReceived(self, datagram, address):
      '''reads an incoming UDP datagram, splits it up, INSERTs into database'''
      try:
         data = csv.reader([datagram.replace('\x00','')]).next() #assumes only one record, strips NULL
         #copies data into a dictionary
         inst=data[0].lstrip('$')
         fields=self.dataProtocols.fields(inst)
         if(len(data)!=len(fields)):
             log.msg('Wrong number of fields in %s expected %i got %i' % (inst,len(fields),len(data)))
             log.msg(data)
             return
         dictdata = dict(zip(fields, data)) 
         instname=self.dataProtocols.protocols[inst][0]['field'].lstrip('$')
         now=int(time.time())
         try:
             if dictdata['utc_time']=='NOW':
                 log.msg('Replace time with %i' % now)
                 dictdata['utc_time']='%i' % now
         except IndexError:
            log.msg('%s has no time rejecting' % data[0].lstrip('$'))
            return # No time !
         try:
             dt=now-int(dictdata['utc_time'])
             if abs(dt)>self.maxTimeError:
                 log.msg('%s time out by %i s rejecting' % (data[0].lstrip('$'),dt))
                 return
         except Exception as e:
             log.msg(str(e))
             return
         self.dataProtocols.add_data(self.cursor, dictdata,('%s' % (instname, )).lower())
         #adds to separate individual-instrument tables; no longer needed, 
         #although it does provide a good error-check in the logs. DW 2013-11-01
         squirrel = 'INSERT INTO %s_%s (%s)' % (instname, self.dataProtocols.protocol_versions[inst], ', '.join(fields))
         processed= []
         for each in fields:
            if dictdata[each] == '':
               processed.append(None)
            else:
               processed.append(dictdata[each])
         if len(data)==len(fields):
            self.cursor.execute(squirrel + ' VALUES (' + (','.join(['%s'] * len(data))) +')', processed)
            log.msg("Insert into %s successful" % data[0].lstrip('$'))
         else:
            log.err("ERROR: Insert into %s failed, mismatched number of fields (%i, expecting %i)" % (data[0].lstrip('$'), len(data), len(self.dataProtocols.fields(data[0].lstrip('$')))))
            log.err(dictdata)
      except _csv.Error:
         log.msg('CSV failed to unpack')
         log.msg(datagram)
  

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

