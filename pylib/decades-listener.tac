# You can run this .tac file directly with:
#    twistd -ny decades-listener.tac

"""
This is a .tac file which starts a UDP multicast listener and listens
for DECADES data.

The important part of this, the part that makes it a .tac file, is
the final root-level section, which sets up the object called 'application'
which twistd will look for
"""
import sys
sys.path.append("/usr/local/lib/decades/pylib") #add deploy python dir to Python path
import os, psycopg2, psycopg2.extensions
from twisted.application import service, internet
from twisted.web import static, server
from listen_udp_twisted import MulticastServerUDP

def getDecadesService():
    """
    Return a service suitable for creating an application object.
    """
    conn = psycopg2.connect (host = "localhost",
                           user = "inflight",
                           password = "wibble",
                           database = "inflightdata")
    #turn off transactions so the incoming INSERTS do not interfere with each other
    conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT) 

    return internet.MulticastServer(50001, MulticastServerUDP(conn))

# this is the core part of any tac file, the creation of the root-level
# application object
application = service.Application("DECADES UDP Listener")

# attach the service to its parent application
service = getDecadesService()
service.setServiceParent(application)
