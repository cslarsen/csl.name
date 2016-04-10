---
layout: post
title: "Commodore 64 development from the command line"
date: 2016-04-10 13:37:13 +0200
updated: 2016-04-10 13:37:13 +0200
categories: programming
disqus: true
tags: c64 assembly
---

If you want to code for the Commodore 64, you can do it easily with a few
command line tools. If you are happy using Sublime, then you can head over to
<a
href="http://dustlayer.com/c64-coding-tutorials/2013/2/10/dust-c64-command-line-tool">Dustlayer</a>
to get a complete package. However, I like doing everything from the command
line.

First, you need to install the ACME assembler and the VICE emulator. On OSX,
these are in Homebrew:

    $ brew install acme vice

Now, to compile

    $ acme --cpu 6502 --outfile foo.prg foo.asm

If you use the dust demo, you can simply type `acme index.asm`, and the output
files will be in `build`.

To make that into a disk image, where `c1514` may be located in
`/Applications/Vice64/tools/c1514`,

    $ c1541 -format diskname,id d64 image_name.d64 -write build/hello_world.prg hello.prg

Now open `x64`, or `open /Applications/Vice64/x64.app`, then type

    LOAD "$",8
    LIST

Make sure that `HELLO.PRG` is there. To load and run it,

    LOAD "HELLO.PRG",8
    RUN

While loading, you may hit Command-W to enter warp speed, then disable it again
before running.

Next steps
----------

  * Create a Makefile
  * Be able to automatically launch x64 and run the program from the command
    line

