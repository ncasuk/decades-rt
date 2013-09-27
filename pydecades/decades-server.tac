# You can run this .tac file directly with:
#    twistd -ny decades-server.tac

"""
This is a .tac file which starts a TCP server and listens
for STAT and PARA requests from the Java client applet

The important part of this, the part that makes it a .tac file, is
the final root-level section, which sets up the object called 'application'
which twistd will look for
"""
import sys
sys.path.append("/usr/local/lib/decades") #add deploy python dir to Python path
import os
from twisted.application import service, internet
from twisted.web import static, server
from pydecades.decades_server import DecadesFactory
from pydecades.database import get_database

def getDecadesServerService():
    """
    Return a service suitable for creating an application object.
    """
    conn = get_database()
    return internet.TCPServer(int(os.environ['DECADESPORT']), DecadesFactory(conn,'pydecades/rt_calcs/HOR_CALIB.DAT'))

# this is the core part of any tac file, the creation of the root-level
# application object
application = service.Application("DECADES Server")

# attach the service to its parent application
service = getDecadesServerService()
service.setServiceParent(application)
