---
layout: post
title: "Embedding binary data in your C and C++ executables"
date: 2016-06-06 19:49:36 +0200
updated: 2016-06-06 19:49:36 +0200
categories: 
disqus: true
tags: linker
---

Applications usually load resources like images from disk. But, in some
situations, it may be better to embed binary data right into the executable
file. Here are a few ways to do that.

In language like C and C++, the straight-forward way to include binary data in
the executable is to convert it to a character array:

    const char data[] = {0x00, 0x01, ...};

While there's nothing wrong with that approach, it requires a tool to convert
binary data to code. A much more elegant solution is to have the linker perform
those operations for you. I'll present several way to solve this problem.

Using the GNU linker `ld`
-------------------------

This is by far the easiest solution, but does not work on Mac OS X, because GNU
ld doesn't fully support it.

Let's say you have an image `cat.png` and want to embed it into your
application. You can create an object file with

    $ ld -r -b binary cat.png -o cat.o

The object file will have three symbols in it,

    $ nm cat.o
    _cat_start
    _cat_end
    _cat_size

To use them from C, declare some extern variables

    extern const char cat_start;
    extern const char cat_end;
    extern const int cat_size;

and add `cat.o` to the compiler:

    $ gcc cat.o program.c -oprogram

If you have a function `display_png_image`, you can simply call

    display_png_image(&cat_start);

Using assembly
--------------

If you don't have GNU ld, or it's not supported fully on your system (like on
OS X) — or, if you want complete control — then you can use an assembler like
<a href="http://www.nasm.us">nasm</a> to create an object file.

To do that, simply create some symbols and use the `incbin` directive. With
`nasm`, you can even use a macro to calculate the size of the binary data:

    bits 64

    section .rodata

    global _cat_start
    global _cat_end
    global _cat_size

    _cat_start:   incbin "cat.png"
    _cat_end:
    _cat_size:    dd $-_cat_start

Compile for OS X with

    $ nasm -fmacho64 cat.asm -o cat.o

or use another value for the format `-f`, and link with your program exactly as
before:

    $ gcc cat.o program.c -o program
