#!/usr/bin/python
#Produces fake 1-second data to test Decades-Server.py
import argparse
import os
import sys
import numbers
import random

from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor, defer
from twisted.internet.task import LoopingCall
from twisted.python import log 
from collections import OrderedDict

#Parse clargs
parser = argparse.ArgumentParser(description='Data simulator for DECADES testing. It defaults to simulating all the DLUs, but you can limit it to a subset using the --DLU option.')

DLUs_list  = ['CORCON', 'GINDAT', 'PRTAFT', 'UPPBBR', 'LOWBBR', 'AERACK','TWCDAT']

#allows choice of DLUs on the command line, defaults to all of them
parser.add_argument('--DLU','--dlu', nargs='*', choices=DLUs_list, default=DLUs_list, help="Which DLU(s) you wish to simulate. (e.g. CORCON, GINDAT) Defaults to all of them.", metavar='DLUNAME')
parser.add_argument('--csv','--CSV', nargs=1, default=[], help="filename of a CSV dump of mergeddata table from a live flight")

args = parser.parse_args()

log.startLogging(sys.stdout)

import psycopg2, datetime, time, math, random
from decades import DecadesDataProtocols
from database import get_database


class DecadesMUDPSender(DatagramProtocol):
    def __init__(self,clargs,flightnum='SIMU'):
        self.flightnum=flightnum
        self.loopObj = None
        self.host = "239.1.4.6"
        self.port = 50001
        self.clargs = clargs
        dp=DecadesDataProtocols()
        self.fakedata={}
        self.csvdata= None
        self.csvoffset=0
        defmap={'double_float': 0.0, 'text': ' ', 'float': 0.0, 'signed_int': 0, 
           'boolean': 0, 'unsigned_int': 0, 'single_float': 0.0 }
        for prot in self.clargs.DLU:    
            inst=OrderedDict()
            for i in dp.protocols[prot+'01']:
                ty=i['type'].replace(">","").replace("<","")
                inst[i['field']]=defmap[ty]
            if('$'+prot+'01' in inst):
                inst['$'+prot+'01']='$'+prot+'01'
            elif(prot+'01' in inst):
                inst[prot+'01']='$'+prot+'01'
            self.fakedata[prot]=inst
    
    def startProtocol(self):
        # Called when transport is connected
        # I am ready to send heart beats
        self.transport.joinGroup(self.host)
        self.loopObj = LoopingCall(self.sendFakeData)
        self.loopObj.start(1, now=False)

    def stopProtocol(self):
        "Called after all transport is torn down"
        pass

    def datagramReceived(self, data, (host, port)):
        pass
        #print "received %r from %s:%d" % (data, host, port)

    def sendPacket(self,udp_string):
        self.transport.write(udp_string, (self.host, self.port))

    def sendFakeData(self):
        if len(self.clargs.csv) > 0:
            #use CSV file for data
            fakedata=self.data_from_csv(self.clargs.csv[0])
        else:
            #no CSV file, make some data up
            fakedata=self.makeupdata()

        instruments = self.fakedata.keys()
        #not always in the same order
        random.shuffle(instruments)
        for inst in instruments:
            for f,v in fakedata[inst].items():
                if f in self.fakedata[inst]:
                    if(isinstance(self.fakedata[inst][f] , numbers.Number) and v==''): 
                        v=None
                    self.fakedata[inst][f]=v
            udp_string=','.join([str(s) for s in self.fakedata[inst].values()])
            #use asynchronous sending so sorder is "realistically" inconsistent
            reactor.callLater(random.uniform(0.1, 1.9),self.sendPacket,udp_string)

    def makeupdata(self):
            timestamp = int(math.floor(time.time()))
            fakedata =  {}
            #create dictionary of fieldnames => simulated value for each DLU
            fakedata['PRTAFT'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum,
               'pressure_alt':int(2625 + (50 * math.sin(timestamp/4.0))), #average 10kft
               'ind_air_speed':int(6624 + (10 * math.cos(timestamp*4))), #average 300kts
               'deiced_temp_flag':False, #alternates between true and false
               'rad_alt':int(random.normalvariate(10000,400))
            }
            fakedata['CORCON'] = {
               'utc_time':timestamp - 17,
               'flight_num':self.flightnum,
               'di_temp':int(240645 + (9000 * math.sin(timestamp/3.0))),
               'ge_dew':int(16618 + (1204 * math.cos(timestamp/3.0))),
               'ndi_temp':int(248797 + (1204 * math.sin(timestamp/3.0))),
               'jw_lwc':int(22300 + (1204 * math.sin(timestamp/3.0))),
               'cabin_t':int(1847435 + (3000 * math.sin(timestamp/6.0))), #3000 ~ 1 degC
               'cabin_p':int(28963 + (50 * math.cos(timestamp/2.5)))
            }
            fakedata['UPPBBR'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum,
               'radiometer_1_sig':int(random.normalvariate(5000,100)),
               'radiometer_1_zero':int(random.normalvariate(4000,100)),
               'radiometer_3_temp':int(900000 + 6500 * math.cos(timestamp/3)),
               'radiometer_3_sig':int(random.normalvariate(50000,10000)),
               'radiometer_3_zero':int(random.normalvariate(50000,10000))
            }
            fakedata['GINDAT'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum,
               'latitude_gin':(0.0 + 5*math.sin(math.radians(timestamp*3))), #A circle centred on capital of Guam
               #'latitude_gin':(52.07 + 5*math.sin(math.radians(timestamp*3))), #A circle centred on capital of Cranfield
               'longitude_gin':(0.0 + 10*math.cos(math.radians(timestamp*3))),
               #'longitude_gin':(-0.61 + 10*math.cos(math.radians(timestamp*3))),
               'heading_gin':(360-((timestamp*3) % 360)),
               'roll_gin':int(random.normalvariate(0,5)),
               'pitch_gin':int(random.normalvariate(0,5)),
               'velocity_north_gin':int((4800 + (5 * math.cos(timestamp*4)))/32 * math.cos(math.radians(360-((timestamp*3) % 360)))), #m/s is kts/2 ish
               'velocity_east_gin':int((4800 + (5 * math.cos(timestamp*4)))/32 * math.sin(math.radians(360-((timestamp*3) % 360)))), #m/s is kts/2 ish
               'velocity_down_gin':int(6 * math.cos(timestamp*3)),
               'altitude_gin':int(1000 + (50 * math.sin(timestamp/4))),  #average 10kft
            }
            fakedata['AERACK'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum,
               'buck_mirr_cln_flag':0,
               'neph_total_blue': 4.773 + 3.0*math.cos(math.radians(timestamp*3.0)),
               'neph_backscatter_blue': 3.75 + 2.81*math.cos(math.radians(timestamp*2.37))
            }
            fakedata['LOWBBR'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum
            }
            fakedata['TWCDAT'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum,
               'twc_detector':random.normalvariate(120,50),
               'status':'11111111'
            }
            return fakedata

    def data_from_csv(self, filename):
        if not self.csvdata:
            #open datafile if not already open
            import csv
            import operator
            log.msg("Using CSV file" + filename)
            f = csv.DictReader(open(filename,'r'))
            self.csvdata = iter(sorted(f, key=operator.itemgetter('utc_time')))
            #get start time offset
            self.csvoffset = int(time.time()) - int(self.csvdata.next()['utc_time'])
            log.msg("Start of data is " + str(self.csvoffset) + " behind current time. Correcting.")

        dataline = self.csvdata.next()
        fakedata = {}
        for each in dataline.keys():
                if each != 'id' and each != 'utc_time':
                    instcode, datum = each.split('_',1)
                    inst = instcode.upper()[0:-2] #i.e. CORCON rather than corcon01
                    if inst not in fakedata:
                        fakedata[inst] = {}
                    if datum=='utc_time' and dataline[each] > '':
                        fakedata[inst][datum] = str(int(dataline[each]) + self.csvoffset)
                        #print(each,  int(time.time()), str(int(dataline[each]) + self.csvoffset))
                    elif datum == '$' + instcode.upper():
                        pass;
                    else:
                        fakedata[inst][datum] = dataline[each]
        return fakedata
        

mudpSenderObj = DecadesMUDPSender(args)

reactor.listenMulticast(0, mudpSenderObj)
reactor.run()
