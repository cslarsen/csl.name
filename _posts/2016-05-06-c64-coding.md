---
layout: post
title: "Commodore 64 development from the command line"
date: 2016-05-06 21:37:13 +0000
updated: 2016-05-06 21:37:13 +0000
categories: programming
disqus: true
tags: c64 assembly
---

<p class="lead">
Have you ever had this nagging feeling that you're less worth because you never
actually coded assembly on the C64? Of course you have! Fortunately, you can
finally do something about it. Here's how to get you started on OS X or Linux.
</p>

(If you happen to be a fan of Sublime, you can head over to
<a href="http://dustlayer.com/c64-coding-tutorials/2013/2/10/dust-c64-command-line-tool">Dustlayer</a>
to get a complete package for that.)

I like tmux and vim/emacs, so I'm going to show how to cross-compile
C64 code from the command line.  

(By the way, here's a fun fact that you may or may not have known: The original
Lucasarts C64 games such as Maniac Mansion were also cross-compiled from UNIX
workstations way back in 1986-87! They cross-compiled and uploaded the code to
running C64s! I was amazed when I heard this, because it's an excellent work
flow, even today! I heard it on the <a
href="https://blog.thimbleweedpark.com">Thimbleweed Park podcast</a>.)

Installing the tools
--------------------

I'll be using the ACME assembler and VICE emulator. On OSX you get those from
Homebrew:

    $ brew install acme vice

On Linux, you may find those in your package manager. If you don't, just
download VICE and compile from source. That way you get the C64 ROMs. (Also,
the last time I compiled VICE, it didn't like to be installed in a non-standard
location). ACME is very old, but should be readily available.

To compile some assembly code on VICE,

    $ acme --cpu 6510 --outfile foo.prg foo.asm

(I'm aware that the 6502 and the C64's 6510 CPUs are supposed to be instruction
set compatible. But I pass `--cpu 6510` as the ISA anyway, just in case there
are some weird cases where they're not the same.)

If you use the dust demo, you can simply type `acme index.asm`, and the output
files will be in `build`.

To make that into a disk image, where `c1514` may be located in
`/Applications/Vice64/tools/c1514`,

    $ c1541 -format diskname,id d64 image_name.d64 -write build/hello_world.prg hello.prg

The Makefile
------------

Here's a makefile that I use with my stuff:

    TARGETS := foo
    C1541 := /Applications/Vice64/tools/c1541
    X64 := open /Applications/Vice64/x64.app

    .PRECIOUS: %.d64

    all: $(TARGETS)

    %.prg: %.asm
      acme --cpu 6510 --format cbm --outfile $@ $<

    %.d64: %.prg
      $(C1541) -format foo,id d64 $@ -write $<

    %: %.d64
      $(X64) $<

    clean:
      rm -f $(TARGETS) *.prg *.d64

The makefile assumes that you have `foo.asm`. By typing

    $ make foo

it will compile `foo.prg`, put it into a C64 disk image and run it.

You can also do it the old fashioned way; Opening `x64` and typing

    LOAD "$",8
    LIST

and then

    LOAD "FOO.PRG",8
    RUN

While loading, you may hit Command-W to enter warp speed, then disable it again
before running.

A BASIC loader
--------------

You can see some examples over at <a href="https://github.com/cslarsen/c64-examples">https://github.com/cslarsen/c64-examples</a>.

The BASIC loader I use there is

    ; A BASIC booter, encodes `10 SYS <address>`.
    ; Macrofied from http://www.pouet.net/topic.php?which=6541

    !source "constants.asm"

    !macro start_at .address {
      * = basic
      !byte $0c,$08,$00,$00,$9e
      !if .address >= 10000 { !byte 48 + ((.address / 10000) % 10) }
      !if .address >=  1000 { !byte 48 + ((.address /  1000) % 10) }
      !if .address >=   100 { !byte 48 + ((.address /   100) % 10) }
      !if .address >=    10 { !byte 48 + ((.address /    10) % 10) }
      !byte $30 + (.address % 10), $00, $00, $00
      * = .address
    }

    ; A cooler example is to write
    ;
    ;   10 SYS <address>: REM <backspaces>Your comment
    ;
    ; When the user types LIST, he will just see
    ;
    ;   10 Your comment
    ;
    ; but still be able to run it.
    ; For this, see http://codebase64.org/doku.php?id=base:acme-macro-tu

The contents of `constants.asm` is simply

    ;; Start of BASIC program
    basic = $0801

    ;; Background color
    bgcol = $d021

    ;; Border color
    bocol = $d020

Example code
------------

To use the BASIC booter, include the file and invoke the macro with `+start_at
<address>`:

    !source "basic-boot.asm"

    +start_at $0900

    ; Set background and border to black
    ldx #$00
    stx bgcol
    stx bocol

    ; Flicker border and background
    .loop
      inc bgcol
      inc bocol
      jmp .loop

It wraps the loader in an ACME macro `start_at`. The main assembly here starts
at `$0900`, meaning it loads super fast: The BASIC loader starts `$0801` and
the rest of the code at `$0900`. Now, the `.PRG` file format simply consists of
a destination address in memory to load the file contents into. If the space
between your BASIC loaer (which *must* start at `$0801`) and your entry point
is huge, then you'll waste space, and the file will take forever to load, even
if you're using warp mode in your emulator. If you need more space, let the
first `.PRG` file be a loader so your program gets up and running quickly.

If you put the above code into `flicker.asm`, using the above makefile you can
now run it by typing

    $ make flicker
    acme --cpu 6510 --format cbm --outfile flicker.prg flicker.asm
    /Applications/Vice64/tools/c1541 -format foo,id d64 flicker.d64 -write flicker.prg
    Unit: 0
    Formatting in unit 8...
    Writing file `FLICKER.PRG' as `FLICKER.PRG' to unit 8.
    open /Applications/Vice64/x64.app flicker.d64
    rm flicker.prg

The output is given below

![Commodore 64 loading flicekr demo](/gfx/post/c64-loading.png)

![Commodore 64 flicker demo](/gfx/post/c64-flicker.png)

What next?
----------

The only thing you need now is a lot of time on your hands, a good C64
reference manual and memory map, and you're set for hours of fun (*after*
you've made a stable raster, of course).
