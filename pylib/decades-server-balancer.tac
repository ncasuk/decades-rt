# You can manually run this .tac file directly in the pylib directory:
#    twistd --rundir=.. -ny decades-server-balancer.tac

"""
This is a .tac file which starts a TCP server which does
round-robin balancing between multiple instances of decades-server.

The important part of this, the part that makes it a .tac file, is
the final root-level section, which sets up the object called 'application'
which twistd will look for
"""
import sys
sys.path.append("/usr/local/lib/decades") #add deploy python dir to Python path
import os
from twisted.application import service, internet
from twisted.web import static, server
from pylib.decades_server_balancer import Balancer

from ConfigParser import SafeConfigParser

def getDecadesServerBalancerService():
    """
    Return a load-balancer service to manage n instances of decades-server.
    """
    parser = SafeConfigParser()
    config = parser.read(['/etc/decades/decades.ini','decades.ini'])
    port = int(parser.get('Servers','port'))
    servers = []
    base_port = int(parser.get('Servers','slave_base_port'))
    for a in range(base_port,base_port + int(parser.get('Servers','slaves'))):
      servers.append(('127.0.0.1',a))
    print servers
    return internet.TCPServer(port, Balancer(servers))

# this is the core part of any tac file, the creation of the root-level
# application object
application = service.Application("DECADES Server Load Balancer")

# attach the service to its parent application
service = getDecadesServerBalancerService()
service.setServiceParent(application)
