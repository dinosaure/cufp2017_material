#!/usr/bin/env python

import os
import select
import subprocess
import urlparse

from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

class S(BaseHTTPRequestHandler):
  def do_GET(self):
    if self.handle_git('GET'):
      return
    return
  def do_HEAD(self):
    if self.handle_git('HEAD'):
      return
    return
  def do_OPTIONS(self):
    self.send_response(200, 'Script output follows')
    self.send_header('Access-Control-Allow-Origin', '*')
    self.send_header('Access-Control-Allow-Methods', 'POST, GET, OPTIONS')
    self.send_header('Access-Control-Allow-Headers', 'content-type, access-control-allow-origin, access-control-allow-methods, access-control-allow-headers')
    self.end_headers()
  def do_POST(self):
    if self.handle_git('POST'):
      return
    return
  def handle_git(self, method):
    print "Start handle git"

    parsed_path = urlparse.urlparse(self.path)
    if not parsed_path.path.startswith('/dev/'):
      return False
    path_no_git = parsed_path.path[len('/dev/'):]
    first_slash = path_no_git.find('/')
    if first_slash < 0:
      return False
    # Assume that all git projects are in /dev/<PROJECT NAME>
    git_project_root = path_no_git[:first_slash]
    path_info = path_no_git[first_slash:]
    print path_info
    print parsed_path.query
    print git_project_root
    env = dict(os.environ)
    env['GIT_HTTP_EXPORT_ALL'] = '1'
    env['REQUEST_METHOD'] = method
    env['QUERY_STRING'] = parsed_path.query
    env['PATH_INFO'] = path_info
    env['GIT_PROJECT_ROOT'] = os.path.join(os.getcwd(), 'dev', git_project_root)
    env['REMOTE_USER'] = 'dinosaure'
    if self.headers.typeheader is None:
        env['CONTENT_TYPE'] = self.headers.type
    else:
        env['CONTENT_TYPE'] = self.headers.typeheader
    length = self.headers.getheader('content-length')
    if length:
        env['CONTENT_LENGTH'] = length
    nbytes = 0
    if length is not None:
      nbytes = int(length)
    self.send_response(200, 'Script output follows')
    self.send_header('Access-Control-Allow-Origin', '*')
    # from CGIHTTPServer.CGIHTTPRequestHandler
    p = subprocess.Popen(['git', 'http-backend'],
                         stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         env=env)
    if method == "POST" and nbytes > 0:
        data = self.rfile.read(nbytes)
    else:
        data = None
    # throw away additional data [see bug #427345]
    while select.select([self.rfile._sock], [], [], 0)[0]:
        if not self.rfile._sock.recv(1):
            break
    stdout, stderr = p.communicate(data)
    self.wfile.write(stdout)
    if stderr:
        self.log_error('%s', stderr)
    p.stderr.close()
    p.stdout.close()
    status = p.returncode
    if status:
        self.log_error("CGI script exit status %#x", status)
    else:
        self.log_message("CGI script exited OK")
    return True

def run(server_class=HTTPServer, handler_class=S, port=8080):
    server_address = ('', port)
    httpd = server_class(server_address, handler_class)
    print 'Starting httpd...'
    httpd.serve_forever()

if __name__ == "__main__":
    from sys import argv

    if len(argv) == 2:
        run(port=int(argv[1]))
    else:
        run()
