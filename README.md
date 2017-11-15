Contains the sources for csl.name
=================================

Building locally
----------------

See `source/README.md`

Building with Docker
--------------------

Setup the Docker image first:

    $ make setup

The best way is to spin up a web container. It builds all files and you can
access the server at `http://localhost:4000`. If you modify any file on the
host system in `source/_drafts` or `source/_posts` the server will update the
posts:

    $ make serve

To just produce the output in `_site` on the host:

    $ make build

To login to container:

    $ make bash

License
-------

Copyright 1996-2017 Christian Stigen Larsen  
Distributed under the GPL v3 (for code, unless otherwise noted; posts have a
different, more liberal, license)
