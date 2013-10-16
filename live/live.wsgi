#!/usr/bin/env python
import web
import sys
sys.path.append('/var/www/decades-live')
import status, flightmanager, parano
        
urls = (
   '/hello/(.*)', 'hello',
   '/stat', status.app,
   '/parano', parano.app,
   '/flightmanager', flightmanager.app   
)
app = web.application(urls, globals(), autoreload=False)
application = app.wsgifunc()


class hello:        
    def GET(self, name):
        if not name: 
            name = 'World'
        return 'Hello, ' + name + '!'

if __name__ == "__main__":
    app.run()
