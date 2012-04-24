#!/usr/bin/env python
############################################################################

# Simple UDP Multicast Client example
# Kyle Robertson
# A Few Screws Loose, LLC
# http://www.afslgames.com
# ra1n@gmx.net
# MulticastClient.py

from twisted.internet.protocol import DatagramProtocol
from twisted.internet import reactor
from twisted.application.internet import MulticastServer
import psycopg2

conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")


class MulticastServerUDP(DatagramProtocol):
    def startProtocol(self):
        print 'Started Listening'
        # Join a specific multicast group, which is the IP we will respond to
        self.transport.joinGroup('225.0.0.0')

    def datagramReceived(self, datagram, address):
      #print repr(address) + ' : ' + repr(datagram)
      data = datagram.split(',')
      cursor = conn.cursor()
      if data[0] == '$CORCON01':
         #print data[1:7] + data[8:10] + data[13:19] + data [21:23] + data[24:26] + data[28:33]
         cursor.execute('INSERT INTO ' + data[0][1:].lower() + '(crio_ident, length_of_data, time_utc, crio_temp, tp_up_down, tp_left_right, fast_temp, ndi_temp, di_temp, tp_p0_s10, tp_top_s10, tp_right_s10, s9_press, nv_lwc, nv_lref, nv_lc, nv_twc, nv_tref, nv_tc, cabin_p, cabin_t, heim_t, heim_c, ge_dew, ge_cont, jw_lwc) VALUES (\'' + data[0][1:] + '\',' +(','.join(data[1:7] + data[8:10] + data[12:19] + data [20:23] + data[24:26] + data[28:33])) + ')')

      if data[0] == '$AERACK01':
         #cursor.execute('INSERT INTO ' + data[0][1:] + '(cRIO_Ident, Length_of_Data,  Time_UTC,  cRIO_Temp, Filter_1_Flow, Filter_1_Pressure, Filter_2_Flow, Filter_2_Pressure, PSAP_Flow, PSAP_LIN, PSAP_LOG, PSAP_Transmission, Neph_Total_Blue, Neph_Total_Green, Neph_Pressure, Neph_Temp, Neph_Backscatter_Blue, Neph_Backscatter_Red, Neph_Backscatter_Green, Neph_Total_Red, Neph_Humidity, Neph_Status, Buck_Mirror_Temperature, Buck_Status, Buck_Pressure, Buck_Coldfinger, Buck_Balance, Buck_Mirror_Flag, Buck_Board_Temp) VALUES (\'' + data[0][1:] + '\','+','.join(data[1:22] + data[24:32]) + ')')
         #Buck data not coming in yet:
         cursor.execute('INSERT INTO ' + data[0][1:].lower() + '(crio_ident, length_of_data,  time_utc,  crio_temp, filter_1_flow, filter_1_pressure, filter_2_flow, filter_2_pressure, psap_flow, psap_lin, psap_log, psap_transmission, neph_total_blue, neph_total_green, neph_pressure, neph_temp, neph_backscatter_blue, neph_backscatter_red, neph_backscatter_green, neph_total_red, neph_humidity, neph_status) VALUES (\'' + data[0][1:] + '\','+','.join(data[1:22]) + ')')

      if data[0] == '$UPPBBR01' or data[0] == '$LOWBBR01': 
         cursor.execute('INSERT INTO ' + data[0][1:].lower() + ' (crio_ident, length_of_data, time_utc, crio_temp, radiometer_1_signal, radiometer_1_temperature, radiometer_2_signal, radiometer_2_temperature, radiometer_3_signal, radiometer_3_temperature, radiometer_4_signal, radiometer_4_temperature, radiometer_1_zero, radiometer_2_zero, radiometer_3_zero, radiometer_4_zero) VALUES (\'' + data[0][1:] + '\','+','.join(data[1:16]) + ')')
   
      if data[0] == '$PRTAFT01': 
         cursor.execute('INSERT INTO ' + data[0][1:].lower() + ' (crio_ident, length_of_data, time_utc, crio_temp, pressure_altitude, indicated_airspeed, radio_altitude, nevzorov_lwc_flag, nevzorov_twc_flag, deiced_temperature_flag, weight_on_wheels_flag, heimann_flag) VALUES (\'' + data[0][1:] + '\','+','.join(data[1:12]) + ')')
   
      conn.commit();
   
      #print data[0]
      cursor.close() 

# Note that the join function is picky about having a unique object
# on which to call join.  To avoid using startProtocol, the following is
# sufficient:
#reactor.listenMulticast(8005, MulticastServerUDP()).join('224.0.0.1')

# Listen for multicast on 224.0.0.1:8005
reactor.listenMulticast(50001, MulticastServerUDP())
reactor.run()
