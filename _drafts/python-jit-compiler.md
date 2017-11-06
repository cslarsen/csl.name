---
layout: post
title: "Writing an x86-64 JIT compiler from scratch. In Python."
date: 2017-11-06 20:05:41 +0100
updated: 2017-11-06 20:05:41 +0100
categories: Python
disqus: true
tags: Python assembly
---

In this post I'll show how to write a rudimentary, native x86-64 JIT-compiler
on UNIX systems like Linux and macOS. All the code in here can be found on
[https://github.com/cslarsen/minijit][github].

While this is normally done in a language like C, it can be done with any
language with a foreign-function interface library (FFI). We'll be using the
built-in module [`ctypes`][ctypes.doc]. An FFI module like `cffi` would make
things a bit easier, but it's unfortunately not yet part of the standard
distribution. Using Python means that we actually have to delve very close to
the OS, meaning that we'll be exposed to its C interface.

Our strategy is simply to grab a piece of memory, write machine code to it,
mark it as executable and then jump to it.

But before we can do anything, we need to load the standard C library.  On
macOS, this can be done with

    import ctypes
    libc = ctypes.cdll.LoadLibrary("libc.dylib")

while on Linux it would be

    import ctypes
    libc = ctypes.cdll.LoadLibrary("libc.so.6")

Before we can fetch a block of memory, we need to know the system's current
page size. This is because only page-aligned memory can be marked as
executable. The page size can be found with the system call `sysconf` using
`_SC_PAGESIZE` as argument.

But what is the integral value of `_SC_PAGESIZE` ? This is defined in the
`sys/mman.h` C header file, and its value will be different on various
operating systems. So we need to find it. This is


Step 2: Fetch a page-aligned block of memory
------------------------------------------------

To mark a block of memory as executable, it needs to be page-aligned. That
means two things: (1) We cannot use `malloc` to fetch memory, because it cannot
give such a guarantee, and (2) we need to fetch a multiple of the system's
page size. In other words, we need to find out how large one memory page is.

To do that, we can use the `sysconf` call with `_SC_PAGESIZE` as argument. We
can set up the func

means we need to grab That
means we cannot use `malloc`, because it cannot give us this guarantee: The
heap manager may sometimes deliver memory crossing page boundaries. Instead
we'll use `mmap`.

The man-page for `mmap` on macOS gives the following synopsis:

    #include <sys/mman.h>

     void *
     mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset);

Delving into

To find the enum values, you can find `mman.h` on your system, use another
foreign-function library like `cffi`
echo '#include <sys/mman.h>' | gcc -E -

Step three: Generate machine code
---------------------------------

Step four: Mark the memory as executable
----------------------------------------

Step five: Call into the code
-----------------------------

[ctypes.doc]: https://docs.python.org/3/library/ctypes.html#module-ctypes
[github]: https://github.com/cslarsen/minijit
