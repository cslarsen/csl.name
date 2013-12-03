"""
Remote syslog client.

Works by sending UDP messages to a remote syslog server. The remote server
must be configured to accept logs from the network.

License: PUBLIC DOMAIN
Author: Christian Stigen Larsen

For more information, see RFC 3164.
"""

import socket

class Facility:
  "Syslog facilities"
  KERN, USER, MAIL, DAEMON, AUTH, SYSLOG, \
  LPR, NEWS, UUCP, CRON, AUTHPRIV, FTP = range(12)

  LOCAL0, LOCAL1, LOCAL2, LOCAL3, \
  LOCAL4, LOCAL5, LOCAL6, LOCAL7 = range(16, 24)

class Level:
  "Syslog levels"
  EMERG, ALERT, CRIT, ERR, \
  WARNING, NOTICE, INFO, DEBUG = range(8)

class Syslog:
  """A syslog client that logs to a remote server.

  Example:
  >>> log = Syslog()
  >>> log.send("hello", Level.WARNING)
  """
  def __init__(self, host="localhost", port=514):
    self.host = host
    self.port = port
    self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

  def send(self, message, level):
    "Send a syslog message to remote host using UDP."
    data = "<%d>%s" % (level + facility*8, message)
    self.socket.sendto(data, (self.host, self.port))

  def warn(self, message, facility=Facility.DAEMON):
    "Send a syslog warning message."
    self.send(message, level=Level.WARNING, facility=facility)

  def notice(self, message, facility=Facility.DAEMON):
    "Send a syslog notice message."
    self.send(message, level=Level.NOTICE, facility=facility)

  def error(self, message, facility=Facility.DAEMON):
    "Send a syslog error message."
    self.send(message, level=Level.ERROR, facility=facility)

  # ... add your own stuff here
