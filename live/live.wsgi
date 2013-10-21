#!/usr/bin/env python
import web
import sys
sys.path.append('/var/www/decades-live')
import status, flight, parano
from render_helper import render_template

urls = (
   '/index', 'index',
   '/stat', status.app,
   '/parano', parano.app,
   '/flight', flight.app   
)
app = web.application(urls, globals(), autoreload=False)
application = app.wsgifunc()


class index:        
    def GET(self):
        web.header('Content-Type','text/html; charset=utf-8', unique=True) 
        return render_template('index.html',
           title=web.ctx.host,
        ).encode('utf-8')

if __name__ == "__main__":
    app.run()
