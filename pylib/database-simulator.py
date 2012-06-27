#!/usr/bin/python
#Produces fake 1-second data to test Decades-Server.py
#Presently produces data 
#515 Time from Midnight (s)
#520 Deiced True Air Temp (K)
import psycopg2, datetime, time, math, random

conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")

#delete previous run
cursor = conn.cursor()
cursor.execute('TRUNCATE TABLE mergeddata;')
cursor.execute('ALTER SEQUENCE mergeddata_id_seq RESTART WITH 1;')
conn.commit()
while 1:
            time.sleep(1)
            seconds_since_midnight = (datetime.datetime.now() - datetime.datetime(datetime.datetime.now().year,datetime.datetime.now().month,datetime.datetime.now().day)).seconds
            timestamp = int(math.floor(time.time()))

            #create dictionary of fieldnames => simulated value
            fakedata = {
               'utc_time':timestamp,
               'prtaft01_utc_time':timestamp,
               'prtaft01_pressure_alt':int(1000 + (50 * math.sin(timestamp/4))), #average 10kft
               'prtaft01_ind_air_speed':int(9600 + (10 * math.cos(timestamp*4))), #average 300kts
               'prtaft01_deiced_temp_flag':True if timestamp%2 else False, #alternates between true and false
               'corcon01_utc_time':timestamp,
               'corcon01_di_temp':int(23000 + (1204 * math.sin(timestamp/3))),
               'corcon01_ge_dew':int(39371 + (1204 * math.cos(timestamp/3))),
               'corcon01_ndi_temp':int(22300 + (1204 * math.sin(timestamp/3))),
               'uppbbr01_utc_time':timestamp,
               'gindat01_utc_time':timestamp,
               'lowbbr01_utc_time':timestamp,
               'aerack01_utc_time':timestamp,
               'uppbbr01_radiometer_3_temp':int(900000 + 6500 * math.cos(timestamp/3)),
               'uppbbr01_radiometer_3_sig':int(random.normalvariate(50000,10000)),
               'uppbbr01_radiometer_3_zero':int(random.normalvariate(50000,10000)),
               'gindat01_latitude_gin':(52.07 + 10*math.sin(math.radians(timestamp*3))),
               'gindat01_longitude_gin':(-0.61 + 10*math.cos(math.radians(timestamp*3))),
               'gindat01_heading_gin':(360-((timestamp*3) % 360))
   

            }
            
            #cursor.execute('INSERT INTO mergeddata (utc_time, prtaft01_utc_time, corcon01_utc_time, uppbbr01_utc_time, gindat01_utc_time, lowbbr01_utc_time, aerack01_utc_time, corcon01_ndi_temp, uppbbr01_radiometer_3_temp, uppbbr01_radiometer_3_sig, uppbbr01_radiometer_3_zero) VALUES (' + str(timestamp) + ', ' + str(timestamp) + ', ' + str(timestamp) + ', ' + str(timestamp) + ',' + str(timestamp) + ',' + str(timestamp) + ',' + str(timestamp) + ', ' + str(911223 + (1204 * math.sin(timestamp/3))) +', ' + str(900000 + 650 * math.cos(timestamp/3)) +', ' + +');')
            cursor.execute('INSERT INTO mergeddata (' + ", ".join(fakedata.keys()) + ') VALUES (' + ", ".join(['%s'] * len(fakedata)) + ')', fakedata.values()) 
            print cursor.query
            conn.commit()

