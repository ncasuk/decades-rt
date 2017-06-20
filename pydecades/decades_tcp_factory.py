#!/usr/bin/python
from twisted.internet.protocol import Protocol, Factory
from twisted.internet import reactor
from twisted.python import log

from sys import stdout

from decades_tcp_listener import DecadesTCPListener
from decades import DecadesDataProtocols
import glob,os,csv

class DecadesTCPFactory(Factory):
    protocol = DecadesTCPListener
    #dataProtocols = DecadesDataProtocols() 
    outfiles = {} 
    location = "/opt/decades/dataformats/TCP"
    dataformats = {} #Dictionary of dataformats
    field_types_map = {('signed_int',4):'i',('signed_int',2):'h',('unsigned_int',4):'I',('unsigned_int',2):'H',('text',4):'4s'}
    default_format={'flight_num':(20,'4s'),'utc_time':(13,'>I'),'packet_length':(9,'>I'),'totalbytes':0}

    def __init__(self):
        dirList=glob.glob(os.path.join(self.location,'*.csv'))
        for proto_path_name in dirList:
            protocol_file_name = os.path.basename(proto_path_name)
            full_path = os.path.join(self.location,protocol_file_name)
            protocolReader = csv.DictReader(open(full_path, 'rb'))
            name=''
            totalbytes=0
            for row in protocolReader:
                if(not(name)):
                    name=row['field']
                    self.dataformats[name]={'name':name[1:]}
                if(row['field'] in ['packet_length','utc_time','flight_num']): # these are the only relavent ones
                    st=''
                    rep=row['representation']
                    repl=int(row['bytes_per_data_point'])
                    if(rep.startswith('>') or rep.startswith('<')):
                        st=rep[0]
                        rep=rep[1:]
                    st+=self.field_types_map[(rep,repl)]
                    self.dataformats[name][row['field']]=(totalbytes,repl,st)
                totalbytes+=int(row['num_of_bytes'])
            self.dataformats[name]['totalbytes']=totalbytes      
        #for each in self.dataProtocols.available():
        #     self.outfiles[each] = {} #dictionary per instrument for fligh #s
        for each in self.dataformats.values():
            self.outfiles[each['name']] = {} #dictionary per instrument for fligh #s

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
