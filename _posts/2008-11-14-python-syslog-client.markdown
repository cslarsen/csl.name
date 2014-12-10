---
layout: post
title:  "A UDP based Python syslog client"
subtitle: "for Windows and UNIX"
date:    2008-11-14 10:53:22 +01:00
updated: 2013-12-03 17:52:00 +01:00
categories: Python
disqus: true
tags: Python syslog UNIX Windows
---

While the Python standard library offers a [syslog][python.syslog] module,
it seem to be a wrapper around the [POSIX syslog system
calls][posix.syslog]. This means you cannot use it to send syslog messages
over the network.

The code below implements a `send` function as described in 
[RFC 3164][rfc3164]. It has been used in production on Windows boxes sending
messages to a Linux syslog server.

For this to work you must configure your syslog daemon to accept logs from
the network.

{% highlight python %}
{% include python/syslog_client.py %}
{% endhighlight %}

If you put it in a file `syslog_client.py` you can use it as a module.

{% highlight python %}
>>> import syslog_client
>>> log = syslog_client.Syslog("remote-host-name")
>>> log.send("howdy", syslog_client.WARNING)
{% endhighlight %}

You can easily extend the class in several ways. E.g., you may want to add
some convenience functions like `warn()`, etc.

[rfc3164]: http://www.ietf.org/rfc/rfc3164.txt
[python.syslog]: http://docs.python.org/2/library/syslog.html
[posix.syslog]: http://pubs.opengroup.org/onlinepubs/007904975/basedefs/syslog.h.html
