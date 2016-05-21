---
layout: post
title: "Mininet Tutorial"
subtitle: "Simulating networks on a single machine"
date: 2016-05-21 21:20:54 +0200
updated: 2016-05-21 21:20:54 +0200
categories: programming networking
disqus: true
tags: sdn python mininet
---

<a href="http://mininet.org">Mininet</a> lets you simulate networks on your own
(real, or virtual) machine. It's extremely useful because you can start *real*
processes on separate nodes in the virtual network and have them communicate.
To top it off, you code everything in Python, and you can even play with
software-defined network: That means you can do stuff like creating your own
non-IP protocols and see how that works out, or you can create *controllers*
that route traffic intelligently — for example, to minimze overall energy use
on the network, or automatically detect and drop malicious traffic.  Finally,
Mininet lets you simulate network problems like congestion and dropped packets.
As a programme, you'll love how you can set up networks with code and try out
interesting approaches to solving networking problems.

But let's start at the beginning.
