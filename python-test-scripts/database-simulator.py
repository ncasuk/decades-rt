#!/usr/bin/python
#Produces fake 1-second data to test Decades-Server.py
#Presently produces data 
#515 Time from Midnight (s)
#520 Deiced True Air Temp (K)
import psycopg2, datetime, time, math

conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata_test")

#delete previous run
cursor = conn.cursor()
cursor.execute('TRUNCATE TABLE scratchdata;')
cursor.execute('ALTER SEQUENCE scratchdata_id_seq RESTART WITH 1;')
conn.commit()
while 1:
            time.sleep(3)
            seconds_since_midnight = (datetime.datetime.now() - datetime.datetime(datetime.datetime.now().year,datetime.datetime.now().month,datetime.datetime.now().day)).seconds
            cursor.execute('INSERT INTO scratchdata (time_from_midnight, deiced_true_airtemp) VALUES (' + str(seconds_since_midnight) + ', ' + str(274.1 + (5 * math.sin(seconds_since_midnight/3))) +');')
            conn.commit()

