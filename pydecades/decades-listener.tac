# You can run this .tac file directly with:
#    twistd -ny decades-listener.tac

"""
This is a .tac file which starts a UDP multicast listener and listens
for DECADES data.

The important part of this, the part that makes it a .tac file, is
the final root-level section, which sets up the object called 'application'
which twistd will look for
"""
import os
from twisted.application import service, internet
from twisted.web import static, server
from pydecades.decades_udp_listener import MulticastServerUDP
from pydecades.database import get_database

def getDecadesService():
    """
    Return a service suitable for creating an application object.
    """
    conn = get_database()
    protocol = MulticastServerUDP(conn)
    service = internet.MulticastServer(50001, protocol)
    return service

# this is the core part of any tac file, the creation of the root-level
# application object
application = service.Application("DECADES UDP Listener")

# attach the service to its parent application
service = getDecadesService()
service.setServiceParent(application)
