""""This class is more or less translated 1:1 from an idl object, which was written by
Dave Tiddeman. The purpose of the class is to communicate with Horace on the ARA.

The connection is a TCP/IP and not a UDP one.
"""


import numpy as np
import os
import pprint
import socket
import struct


class Horace( object ):
	"""Class to communicate with horace. Query status and query parameters"""
	
	def __init__( self, host=None, port=None, timeout=None ):
		"""host: host name (default: horace 192.168.101.71)
		   port: port number (default: 1500) 
		"""
		
		self.derindex = 0
		self.dercount = 0
		self.mapstatus = 0
		self.status = { 'map':      0,
						'derindex': 0,
						'dercount': 0,
						'time':     0,
						'hdg':      0.0,
  						'spr':      0.0,
    		  			'phgt':     0.0,
			  			'tas':      0.0,
			  			'tat':      0.0,
  			  			'dp':       0.0,
  			  			'wspd':     0.0,
    			  		'wang':     0.0,
  				     	'lat':      0.0,
  				     	'lon':      0.0,
    				    'flno':     '' }

		if not host:
			#self.host = 'horace'
			self.host = '192.168.101.71' # horace IP addresse
		else:
			self.host = host
		
		if not port:
			self.port = 1500
		else:
			self.port = int( port )
		
		self.buf = 256	
		self.addr = ( self.host, self.port )
		
		

	def connect( self, timeout=None ):
		if not timeout:
			timeout = 10
		
		self.TCPSock = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
		self.TCPSock.settimeout( timeout )
		
		self.TCPSock.connect( self.addr )		



	def getstatus( self ):
		"""OUTPUTS:
                 a dictionary containing horace status info
		"""

  		# establish connection to horace
  		#self.TCPSock.connect( self.addr )
  		
  		# Horace needs to get a I got this from the idl code from Dave Tiddeman
  		lonarr63 = ''
  		for i in range(63):
  			lonarr63 = lonarr63 + struct.pack( '>l', 0 )
  		msg = 'STAT' + lonarr63
  		
  		self.TCPSock.send( msg )

		return self.readstatus()



	def readstatus( self, stream=None ):
		
		if not stream:		
			stream = self.TCPSock.recv( 64 )

		# binary format of status msg send by horace
		#  1x byte
		#  2x 16bit unsigned integer
		# 11x 4byte float
		#  4x characters(string of length 4)
		fmt = '>B 2H 11f 4s'
		
		self.stream = stream	

		# update status dictionary	
		( self.status['map'], 
		self.status['derindex'],
		self.status['dercount'],
		self.status['time'],
		self.status['hdg'],
		self.status['spr'],
		self.status['phgt'],
		self.status['tas'],
		self.status['tat'],
		self.status['dp'],	
		self.status['wspd'],
		self.status['wang'],
		self.status['lat'],
		self.status['lon'],
		self.status['flno'] ) = struct.unpack( fmt, stream )

		#debugging
		#pprint.pprint( self.status )  		
		#print( len(stream))
		#return data

		
		
	def getparas( self, start, stop, paras ):
		"""
			start : time in seconds past midnight (LONG)
			stop  : time in seconds pas midnight (LONG)
			paras : list of parameters to read (LONARR)
		"""
		
		if start > 0:
			start = self.status['derindex'] - int((self.status['time'] - start) / 3.0)
		if start < 0:
			start = 1
		if start > self.status['derindex']:
			start = self.status['derindex']
		
		if stop > 0:
			stop = self.status['derindex'] - int((self.status['time'] - stop) / 3.0)
		if stop < start:
			stop = start
		if stop > self.status['derindex']:
			stop = self.status['derindex']
		
		
		# debugging
		#print( '#############' )
		#print( start )
		#print( stop )
		#print( '#############' )		
		#start = 2000
		#stop = 2100
				
		#n_p = len( paras )	
		q = []
		for i in range(63): q.append( 0 )		
		
		# debugging
		q[0] = start
		q[1] = stop
		q[2] = len(paras)
		
		
		ix = 3 #offset 
		for para in paras:
			q[ix] = para
			ix = ix + 1
					
		fmt = '>4s 63l'
		msg = ''
		for i in range(len(q)):
			msg = msg + struct.pack( '>l', q[i] )
		msg = 'PARA' + msg
		
		fmt = '>4s 63l'
		#msg = struct.pack(fmt, 'PARA', 960, 1293, 3, 515, 622, 658, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
		
		#debugging
		self.para_msg = msg

		#return None

  		#self.TCPSock.connect( self.addr )
  		self.TCPSock.send( msg )
		
		#print( struct.unpack( 'fmt', msg))

		# set buffer
		buffer = 256
				
		size = 0	#variable to hold 
		stream = ''
		
		while size < 61:
			stream = stream + self.TCPSock.recv( buffer )
			size = len( stream )

		# update status information				
		self.readstatus( stream=stream[0:53] )
		
		b = struct.unpack( '>2l', stream[53:61] )
		
		print( '###########' )
		print( stop, start )
		print( b )
		print( '###########' )
		
		# calculate the stream length that is going to be received
		size_tot = 53 + 8 + ( len(paras) * b[1] * 4 )
		
		print(size_tot)
		while size < size_tot:
			stream = stream + self.TCPSock.recv( buffer )
			size = len( stream )
			#print(len(stream))
			
			
		rows = (len(stream)-61)/ (4 * len(paras) )
		cols = len( paras )
		fmt = '>' + str( int( rows * cols)) + 'f'
		
		#data = np.array( fmt, stream[61:] )
		
		print( fmt )
		print( rows )
		print( cols )
		self.stream = stream
		data = struct.unpack( fmt, stream[61:] )
		
		#return data
		
		data = np.array( data )
		data = data.reshape( cols, rows )
				
		#data = np.array( list( struct.unpack( fmt, stream[61:] )))
		
		#data.reshape( cols, rows ).transpose()

		print("DONE")
		return data
		


	def cleanup( self ):
		self.TCPSock.close()



	def close( self ):
		self.TCPSock.close()



if __name__ == '__main__':
	pass
	


	
runtest = True

if runtest:
	#  515: time
	#  576  spr static pressure
	#  577: psp, pitot static pressure
	#  609: gps alt
	#  608: gps lon
	#  607: gps lat
	#  657: teco no
	#  658: teco no2
	#  659: teco nox
	#  574: Ozone
	#  588: CO
	params = [ 515, 576, 577, 609, 608, 607, 657, 658, 659, 574, 588 ]
	
	hor = Horace()
	hor.connect()
	hor.getstatus()
	
	t = hor.status['time']
	
	stime = t - 1500
	etime = t
	
	data = hor.getparas( stime, etime, params )
	#hor.close()

	t        = data[0][:]
	spr      = data[1][:]
	psp      = data[2][:]
	gps_alt  = data[3][:]
	gps_lon  = data[4][:]
	gps_lat  = data[5][:]
	teco_no  = data[6][:]
	teco_no2 = data[7][:]
	teco_nox = data[8][:]
	ozone    = data[9][:]
	co       = data[10][:]


	#hor.close()
	#print( data.size )
	

