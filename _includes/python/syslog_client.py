"""
Remote syslog client.

Works by sending UDP messages to a remote syslog server. The remote server
must be configured to accept logs from the network.

License: PUBLIC DOMAIN
Author: Christian Stigen Larsen

For more information, see RFC 3164.
"""

import socket

# SYSLOG FACILITIES

KERN = 0
USER = 1
MAIL = 2
DAEMON = 3
AUTH = 4
SYSLOG = 5
LPR = 6
NEWS = 7
UUCP = 8
CRON = 9
AUTHPRIV = 10
FTP = 11
LOCAL0 = 16
LOCAL1 = 17
LOCAL2 = 18
LOCAL3 = 19
LOCAL4 = 20
LOCAL5 = 21
LOCAL6 = 22
LOCAL7 = 23

# SYSLOG LEVELS

EMERG = 0
ALERT = 1
CRIT = 2
ERR = 3
WARNING = 4
NOTICE = 5
INFO = 6
DEBUG = 7

class Syslog:
  """A syslog client that logs to a remote server.

  Example:
  >>> log = Syslog()
  >>> log.send("hello", WARNING)
  """

  def __init__(self, host="localhost", port=514):
    self.host = host
    self.port = port
    self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

  def send(self, message, level=NOTICE, facility=DAEMON):
    data = "<%d>%s" % (level + facility*8, message)
    self.socket.sendto(data, (self.host, self.port))
