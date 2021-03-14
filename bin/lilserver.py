#!/usr/bin/env python3
# lilserver.py --- http.server for local needs

from pathlib import Path

import http.server
import os
import socketserver

HOST = "127.0.0.1"
PORT = 1993

os.chdir(Path(os.getenv("MY")) / "html")

Handler = http.server.SimpleHTTPRequestHandler

with socketserver.TCPServer((HOST, PORT), Handler) as httpd:
    print("serving at", HOST, ", port", PORT)
    httpd.serve_forever()
