#!/usr/bin/env python
# -*- coding: utf-8 -*-
# kate: replace-tabs on; replace-tabs-save on;

import os
import BaseHTTPServer
import SimpleHTTPServer

try:
    os.chdir("build")
    HandlerClass = SimpleHTTPServer.SimpleHTTPRequestHandler
    ServerClass = BaseHTTPServer.HTTPServer
    HandlerClass.protocol_version = "HTTP/1.0"
    httpd = ServerClass(('', 8000), HandlerClass)
    print "Serving on http://localhost:8000"
    print "Press Ctrl+C to quit."
    httpd.serve_forever()
except KeyboardInterrupt:
    print "Stopping webserver."
