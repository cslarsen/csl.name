Contains the sources for csl.name
=================================

This repo contains the source code for https://csl.name, which is (mostly) a
static site. When publishing, the makefile will compress and minify everything
before rsyncing.

The `make -j dist` target scores very high on Google's PageSpeed index for both
mobile and desktop. You need to install several tools to make it work — see in
the Makefile.

Prerequisites
-------------

You need Jekyll; basically you need everything in `Gemfile`.

You also need tools like `yuicompressor`, perl and so on.

Building
--------

  * `make -j` builds the site locally in `_site`
  * `make -j serve` builds and serves HTTP on localhost:4000
  * `make -j dist` builds and publishes to the web

License
-------

Copyright 1996-2016 Christian Stigen Larsen  
Distributed under the GPL v3 (for code; posts have a different, more liberal,
license)
