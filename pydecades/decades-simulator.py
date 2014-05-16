#!/usr/bin/python
#Produces fake 1-second data to test Decades-Server.py
import argparse
from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor
from twisted.internet.task import LoopingCall
from twisted.python import log 
from collections import OrderedDict

#Parse arguments
parser = argparse.ArgumentParser(description='Data simulator for DECADES testing. It defaults to simulating all the DLUs, but you can limit it to a subset using the --DLU option.')

DLUs_list  = ['CORCON', 'GINDAT', 'PRTAFT', 'UPPBBR', 'LOWBBR', 'AERACK','TWCDAT']

#allows choice of DLUs on the command line, defaults to all of them
parser.add_argument('--DLU','--dlu', nargs='*', choices=DLUs_list, default=DLUs_list, help="Which DLU(s) you wish to simulate. (e.g. CORCON, GINDAT) Defaults to all of them.", metavar='DLUNAME')

args = parser.parse_args()

import psycopg2, datetime, time, math, random
from decades import DecadesDataProtocols
from database import get_database


class DecadesMUDPSender(DatagramProtocol):
    def __init__(self,DLUs,flightnum='SIMU'):
        self.flightnum=flightnum
        self.loopObj = None
        self.host = "239.1.4.6"
        self.port = 50001
        dp=DecadesDataProtocols()
        self.fakedata={}
        defmap={'double_float': 0.0, 'text': ' ', 'float': 0.0, 'signed_int': 0, 
           'boolean': 0, 'unsigned_int': 0, 'single_float': 0.0 }
        for prot in DLUs:    
            inst=OrderedDict()
            for i in dp.protocols[prot+'01']:
                ty=i['type'].replace(">","").replace("<","")
                inst[i['field']]=defmap[ty]
            if('$'+prot+'01' in inst):
                inst['$'+prot+'01']='$'+prot+'01'
            elif(prot+'01' in inst):
                inst[prot+'01']='$'+prot+'01'
            self.fakedata[prot]=inst
            print inst
    
    def startProtocol(self):
        # Called when transport is connected
        # I am ready to send heart beats
        self.transport.joinGroup(self.host)
        self.loopObj = LoopingCall(self.sendFakeData)
        self.loopObj.start(1, now=False)

    def stopProtocol(self):
        "Called after all transport is teared down"
        pass

    def datagramReceived(self, data, (host, port)):
        pass
        #print "received %r from %s:%d" % (data, host, port)

    def sendFakeData(self):
        fakedata=self.makeupdata()
        for inst in self.fakedata.keys():
            for f,v in fakedata[inst].items():
                self.fakedata[inst][f]=v
            udp_string=','.join([str(s) for s in self.fakedata[inst].values()])
            print udp_string
            self.transport.write(udp_string, (self.host, self.port))

    def makeupdata(self):
            timestamp = int(math.floor(time.time()))
            fakedata =  {}
            #create dictionary of fieldnames => simulated value for each DLU
            fakedata['PRTAFT'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum,
               'pressure_alt':int(1000 + (50 * math.sin(timestamp/4))), #average 10kft
               'ind_air_speed':int(9600 + (10 * math.cos(timestamp*4))), #average 300kts
               'deiced_temp_flag':True, #alternates between true and false
               'rad_alt':int(random.normalvariate(10000,400))
            }
            fakedata['CORCON'] = {
               'utc_time':timestamp,
               'flight_num':self.flightnum,
               'di_temp':int(240000 + (9000 * math.sin(timestamp/3))),
               'ge_dew':int(39371 + (1204 * math.cos(timestamp/3))),
               'ndi_temp':int(22300 + (1204 * math.sin(timestamp/3))),
               'jw_lwc':int(22300 + (1204 * math.sin(timestamp/3)))
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
               'flight_num':self.flightnum
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

mudpSenderObj = DecadesMUDPSender(args.DLU)

reactor.listenMulticast(0, mudpSenderObj)
reactor.run()
