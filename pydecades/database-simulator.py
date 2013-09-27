#!/usr/bin/python
#Produces fake 1-second data to test Decades-Server.py
import argparse

#Parse arguments
parser = argparse.ArgumentParser(description='Data simulator for DECADES testing. It defaults to simulating all the DLUs, but you can limit it to a subset using the --DLU option.')

DLUs_list  = ['CORCON', 'GINDAT', 'PRTAFT', 'UPPBBR', 'LOWBBR', 'AERACK']

#allows choice of DLUs on the command line, defaults to all of them
parser.add_argument('--DLU','--dlu', nargs='*', choices=DLUs_list, default=DLUs_list, help="Which DLU(s) you wish to simulate. (e.g. CORCON, GINDAT) Defaults to all of them.", metavar='DLUNAME')

args = parser.parse_args()

import psycopg2, datetime, time, math, random
from decades import DecadesDataProtocols
from database import get_database

conn = get_database()
dataProtocols = DecadesDataProtocols() 
#delete previous run
cursor = conn.cursor()
cursor.execute('TRUNCATE TABLE mergeddata;')
cursor.execute('ALTER SEQUENCE mergeddata_id_seq RESTART WITH 1;')
conn.commit()
while 1:
            seconds_since_midnight = (datetime.datetime.now() - datetime.datetime(datetime.datetime.now().year,datetime.datetime.now().month,datetime.datetime.now().day)).seconds
            timestamp = int(math.floor(time.time()))
            flightnum = 'SIMU'
           
            fakedata =  {}
            #create dictionary of fieldnames => simulated value for each DLU
            fakedata['PRTAFT'] = {
               'utc_time':timestamp,
               'flight_num':flightnum,
               'pressure_alt':int(1000 + (50 * math.sin(timestamp/4))), #average 10kft
               'ind_air_speed':int(9600 + (10 * math.cos(timestamp*4))), #average 300kts
               'deiced_temp_flag':True if timestamp%2 else False, #alternates between true and false
               'rad_alt':int(random.normalvariate(10000,400))
            }
            fakedata['CORCON'] = {
               'utc_time':timestamp,
               'flight_num':flightnum,
               'di_temp':int(23000 + (1204 * math.sin(timestamp/3))),
               'ge_dew':int(39371 + (1204 * math.cos(timestamp/3))),
               'ndi_temp':int(22300 + (1204 * math.sin(timestamp/3))),
               'jw_lwc':int(22300 + (1204 * math.sin(timestamp/3)))
            }
            fakedata['UPPBBR'] = {
               'utc_time':timestamp,
               'flight_num':flightnum,
               'radiometer_1_sig':int(random.normalvariate(5000,100)),
               'radiometer_1_zero':int(random.normalvariate(4000,100)),
               'radiometer_3_temp':int(900000 + 6500 * math.cos(timestamp/3)),
               'radiometer_3_sig':int(random.normalvariate(50000,10000)),
               'radiometer_3_zero':int(random.normalvariate(50000,10000))
            }
            fakedata['GINDAT'] = {
               'utc_time':timestamp,
               'flight_num':flightnum,
               'latitude_gin':(52.07 + 10*math.sin(math.radians(timestamp*3))), #A circle centred on Cranfield
               'longitude_gin':(-0.61 + 10*math.cos(math.radians(timestamp*3))),
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
               'flight_num':flightnum
            }
            fakedata['LOWBBR'] = {
               'utc_time':timestamp,
               'flight_num':flightnum
            }
            for DLU in args.DLU:
               time.sleep(random.gauss(0.15,0.03)) #makes "random" gaps between data
               dataProtocols.add_data(cursor, fakedata[DLU], DLU +'01')
