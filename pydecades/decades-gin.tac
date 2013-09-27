# You can run this .tac file directly with:
#    twistd -ny decades-gin.tac

"""
This is a .tac file which starts a TCP server and listens
on one network interface (usually eth0) for incoming GIN data
and resends in on eth1 for legacy instruments while
processing it for the live data system and logging it.

The important part of this, the part that makes it a .tac file, is
the final root-level section, which sets up the object called 'application'
which twistd will look for
"""
import sys
sys.path.append("/usr/local/lib/decades") #add deploy python dir to Python path
import os
from twisted.application import service, internet
from twisted.web import static, server
from pydecades.decades_gin_forward import GINClientFactory
from pydecades.database import get_database

from ConfigParser import SafeConfigParser

def getDecadesGINService():
   """
   Return a service that manages GIN TCP data
   """
   parser = SafeConfigParser()
   config = parser.read(['/etc/decades/decades.ini','decades.ini'])
   
   GinPort = int(parser.get('GIN', 'port'))  
   GinAddress = (parser.get('GIN', 'address'))  
   GinOutPort = int(parser.get('GIN', 'outport'))  
   GinOutAddress = (parser.get('GIN', 'outaddress'))  
      
   conn = get_database()
   return internet.TCPClient(GinAddress, GinPort, GINClientFactory(outaddress=GinOutAddress, outport=GinOutPort))

# this is the core part of any tac file, the creation of the root-level
# application object
application = service.Application("DECADES TCP GIN processor")

# attach the service to its parent application
service = getDecadesGINService()
service.setServiceParent(application)
