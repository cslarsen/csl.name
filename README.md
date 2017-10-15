Contains the sources for csl.name
=================================

This repo contains the source code for https://csl.name, which is (mostly) a
static site. When publishing, the makefile will compress and minify everything
before rsyncing.

The `make -j dist` target scores very high on Google's PageSpeed index for both
mobile and desktop. You need to install several tools to make it work — see in
the Makefile.

Requirements
------------

  * Everything in the Gemfile
  * GNU parallel
  * Jekyll
  * chibi-scheme
  * optipng
  * perl
  * pngfix
  * python
  * yuicompressor

Building
--------

Install packages with

    $ gem install bundler
    $ bundle install

Then build everything with

  * `make -j` builds the site locally in `_site`
  * `make -j serve` builds and serves HTTP on localhost:4000
  * `make -j dist` builds and publishes to the web

License
-------

Copyright 1996-2016 Christian Stigen Larsen  
Distributed under the GPL v3 (for code; posts have a different, more liberal,
license)
