#!/usr/bin/python
# Echo client program
import socket, struct

HOST = '127.0.0.1'    # The remote host
PORT = 1500              # The same port as used by the server
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((HOST, PORT))
s.send('STAT')
status = s.recv(1024)
print 'Received', repr(status)
para = struct.pack('>4c5i','P','A','R','A',1,-1,2,515,520)
s.send(para)
paraback = s.recv(1024)
print 'Received', repr(paraback)

