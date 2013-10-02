#!/usr/bin/env python
def application(environ, start_response):
    start_response('200 OK', [('Content-Type', 'text/plain')])
    yield '<h1>Hello World</h1>\n'
