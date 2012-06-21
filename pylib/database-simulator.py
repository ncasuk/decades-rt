#!/usr/bin/python
#Produces fake 1-second data to test Decades-Server.py
#Presently produces data 
#515 Time from Midnight (s)
#520 Deiced True Air Temp (K)
import psycopg2, datetime, time, math

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
            
            cursor.execute('INSERT INTO mergeddata (utc_time, prtaft01_utc_time, corcon01_utc_time, uppbbr01_utc_time, gindat01_utc_time, lowbbr01_utc_time, aerack01_utc_time, corcon01_ndi_temp) VALUES (' + str(timestamp) + ', ' + str(timestamp) + ', ' + str(timestamp) + ', ' + str(timestamp) + ',' + str(timestamp) + ',' + str(timestamp) + ',' + str(timestamp) + ', ' + str(911223 + (1204 * math.sin(timestamp/3))) +');')
            conn.commit()

