# You can run this .tac file directly with:
#    twistd -ny decades-tcp-listener.tac

"""
This is a .tac file which starts a TCP listener and listens
for the noninstantaneous data suitable for archiving

The important part of this, the part that makes it a .tac file, is
the final root-level section, which sets up the object called 'application'
which twistd will look for
"""
import os
from twisted.application import service, internet
from twisted.web import static, server
from pydecades.decades_tcp_factory import DecadesTCPFactory
from pydecades.database import get_database

from pydecades.configparser import DecadesConfigParser

def getDecadesTCPListenerService():
    """
    Return a service suitable for creating an application object.
    """
    conn = get_database()
    parser = DecadesConfigParser()
    tcp_port = int(parser.get('TCP_Listener','tcp_port'))
    return internet.TCPServer(tcp_port, DecadesTCPFactory())

# this is the core part of any tac file, the creation of the root-level
# application object
application = service.Application("DECADES TCP listener")

# attach the service to its parent application
service = getDecadesTCPListenerService()
service.setServiceParent(application)
