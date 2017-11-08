---
layout: post
title: "A basic x86 JIT compiler from scratch in Python"
date: 2017-11-06 20:05:41 +0100
updated: 2017-11-06 20:05:41 +0100
categories: Python
disqus: true
tags: Python assembly
---

In this post I'll show how to write a rudimentare, native x86-64 JIT-compiler
in Python. It has been written for UNIX systems, and should work for Linux and
macOS, but you should be able to transfer it quite easily to Windows as well.
All the code in here can be found on [https://github.com/cslarsen/minijit][github].

Our aim is simply to patch the following code with a constant, put it in a
block of memory and execute it.

    48 b8 ed ef be ad de  movabs $0xdeadbeefed, %rax
    00 00 00
    48 0f af c7           imul   %rdi,%rax
    c3                    retq

In other words, we will be dealing with the left hand side of the disassembly
above — in machine code. The fifteen bytes encode a function that multiplies
its argument in the RDI register with the constant `0xdeadbeefed`. This is the
constant that we'll specialize. It will serve as a simple way to check that we
are doing everything correctly.

To fetch a block of memory that we can later mark as executable, we must be
sure that it is page-aligned. To simplify things, we'll just use
[`mmap`][mmap.man] and allocate a whole memory page. That's usually 4096 bytes,
depending on your system. After that, we use [`mprotect`][mprotect.man] to
change it to executable memory.

We will use the [`ctypes`][ctypes.doc] module in Python to load the standard C
library, so that we can call those functions. We'll also need a few
system-dependent constants. I found those by digging into the header files. You
can also construct a small C program to print them out. The only place they
exist is in the header files. A more elegant solution would be to use a
foreign-function interface like [`cffi`][cffi.github], because it is able to
parse the header files directly. However, it is not in the default Python
distribution, so we'll stick to ctypes.

Preliminary step
----------------

Before we can do anything, we need to load the standard C library.

    import ctypes

    if sys.platform.startswith("darwin"):
        libc = ctypes.cdll.LoadLibrary("libc.dylib")
        # ...
    elif sys.platform.startswith("linux"):
        libc = ctypes.cdll.LoadLibrary("libc.so.6")
        # ...

To find the pagesize, we'll call `sysconf(_SC_PAGESIZE)`. The `_SC_PAGESIZE`
constant is 29 on macOS and 30 on Linux. We'll just hardcode those in our
program. In addition, we'll define a few extra constants.

    import ctypes

    if sys.platform.startswith("darwin"):
        libc = ctypes.cdll.LoadLibrary("libc.dylib")
        _SC_PAGESIZE = 29
        MAP_ANONYMOUS = 0x1000
        MAP_PRIVATE = 0x0002
        PROT_EXEC = 0x04
        PROT_NONE = 0x00
        PROT_READ = 0x01
        PROT_WRITE = 0x02
        MAP_FAILED = -1 # voidptr actually
    elif sys.platform.startswith("linux"):
        libc = ctypes.cdll.LoadLibrary("libc.so.6")
        _SC_PAGESIZE = 30
        MAP_ANONYMOUS = 0x20
        MAP_PRIVATE = 0x0002
        PROT_EXEC = 0x04
        PROT_NONE = 0x00
        PROT_READ = 0x01
        PROT_WRITE = 0x02
        MAP_FAILED = -1 # voidptr actually

Although not strictly required, it is very useful to tell ctypes the signature
of the functions we'll use. That way, we'll get exceptions if we mix invalid
types.

    # Set up sysconf
    sysconf = libc.sysconf
    sysconf.argtypes = [ctypes.c_int]
    sysconf.restype = ctypes.c_long

This tells ctypes that `sysconf` is a function that takes a single integer and
produces a long integer. To actually fetch the page size, we can now just cal

    PAGESIZE = sysconf(_SC_PAGESIZE)

The machine code will be interpreted as unsigned 8-bit bytes, so we need to
declare a new ctypes pointer:

    # 8-bit unsigned pointer type
    c_uint8_p = ctypes.POINTER(ctypes.c_uint8)

Below we just dish out the remaining signatures for the functions that we'll
use. For error reporting, it's good to have the `strerror` function available.
We'll use `munmap` to destroy the machine code block after we're done with it.
It lets the operating system reclaim that memory.

    strerror = libc.strerror
    strerror.argtypes = [ctypes.c_int]
    strerror.restype = ctypes.c_char_p

    mmap = libc.mmap
    mmap.argtypes = [ctypes.c_void_p,
                     ctypes.c_size_t,
                     ctypes.c_int,
                     ctypes.c_int,
                     ctypes.c_int,
                     # Below is actually off_t, which is 64-bit on macOS
                     ctypes.c_int64]
    mmap.restype = c_uint8_p

    munmap = libc.munmap
    munmap.argtypes = [ctypes.c_void_p, ctypes.c_size_t]
    munmap.restype = ctypes.c_int

    mprotect = libc.mprotect
    mprotect.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_int]
    mprotect.restype = ctypes.c_int

At this point, with all the boiler-plate code, I have to admit it's hard to
justify writing this in Python rather than pure C. We're working with a C
library at the ABI-level, which can be quite flaky. Also, our code is larger
than an equivalent C code up to this point. But down the line, Python will make
it vastly simpler to experiment with JIT-compilation.

Helper functions
----------------

Now we're ready to write the `mmap` wrapper.

    def create_block(size):
        ptr = mmap(0, size, PROT_WRITE | PROT_READ,
                MAP_PRIVATE | MAP_ANONYMOUS, 0, 0)

        if ptr == MAP_FAILED:
            raise RuntimeError(strerror(ctypes.get_errno()))

        return ptr

This fuction uses `mmap` to allocate page-aligned memory for us. We mark the
memory region as readable and writable with the PROT flags, and we also mark it
as private and anynomous. The [`mmap` manual page][mmap.man] covers the
details. If the `mmap` call fails, we raise it as a Python error.

To mark memory as executable,

    def make_executable(block, size):
        if mprotect(block, size, PROT_READ | PROT_EXEC) != 0:
            raise RuntimeError(strerror(ctypes.get_errno()))

With this `mprotect` call, we mark the memory region as readable and
executable. We could also have made it writable as well, but some systems will
deny executing code in memory that is writable. This is sometimes called the
[W^X][wx.wiki].

To destroy the memory block, we'll use

    def destroy_block(block, size):
        if munmap(block, size) == -1:
            raise RuntimeError(strerror(ctypes.get_errno()))
        del block

A machine code example
----------------------

We're now ready to create an insanely simple piece of JIT code. It's a
multiplier, but it hard-codes the number to multiply with. By definition, we're
specializing a piece of machine code here, even if it is a weak one.

The code contains a *lot* of superfluous stuff like a function prologue for
setting up the stack and so on. We don't really need all of that, but I just
copied the entire thing from a C routine that I disassembled. You can do it
by putting the following in `multiply.c`:

    #include <stdint.h>

    uint64_t multiply(uint64_t n)
    {
      return n*0xdeadbeefedULL;
    }

Compile with something like

    $ gcc -Os -fPIC -shared -fomit-frame-pointer multiply.c -olibmultiply.so

Then dump the machine code

    $ objdump -d libmultiply.so
    ...
    0000000000000f71 <_multiply>:
     f71:	48 b8 ed ef be ad de 	movabs $0xdeadbeefed,%rax
     f78:	00 00 00 
     f7b:	48 0f af c7          	imul   %rdi,%rax
     f7f:	c3                   	retq

The only thing to notice here is that the constant `0xdeadbeefed` is encoded in
little-endian format. We'll now create a function that multiplies with a
constant that we choose from Python.

    def make_multiplier(block, multiplier):
        # Encoding of: movabs <multiplier>, rdx
        block[0] = 0x48
        block[1] = 0xba

        # Little-endian encoding of multiplier
        block[2] = (multiplier & 0x00000000000000ff) >>  0
        block[3] = (multiplier & 0x000000000000ff00) >>  8
        block[4] = (multiplier & 0x0000000000ff0000) >> 16
        block[5] = (multiplier & 0x00000000ff000000) >> 24
        block[6] = (multiplier & 0x000000ff00000000) >> 32
        block[7] = (multiplier & 0x0000ff0000000000) >> 40
        block[8] = (multiplier & 0x00ff000000000000) >> 48
        block[9] = (multiplier & 0xff00000000000000) >> 56

        # Encoding of: mov rdi, rax
        block[10] = 0x48
        block[11] = 0x89
        block[12] = 0xf8

        # Encoding of: imul rdx, rax
        block[13] = 0x48
        block[14] = 0x0f
        block[15] = 0xaf
        block[16] = 0xc2

        # Encoding of: retq
        block[17] = 0xc3

        # Return a ctypes function with the right prototype
        function = ctypes.CFUNCTYPE(ctypes.c_int64)
        function.restype = ctypes.c_int64
        return function

The most interesting part here is copying the constant to multiply with,
starting at index 11. It needs to be encoded in little-endian order, so we do
that here. The last part creates a ctypes function type that it returns.

Startup code
------------

Now the rest is simply

    def main():
        if len(sys.argv) > 1:
            arg = int(sys.argv[1])
        else:
            arg = 2

        print("Pagesize: %d" % PAGESIZE)

        print("Allocating one page of memory")
        block = create_block(PAGESIZE)

        print("JIT-compiling a native mul-function w/arg %d" % arg)
        function_type = make_multiplier(block, arg)

        print("Making function block executable")
        make_executable(block, PAGESIZE)
        mul = function_type(ctypes.cast(block, ctypes.c_void_p).value)

        print("Testing function")
        for i in range(10):
            print("mul(%d) = %d" % (i, mul(i)))

        print("Deallocating function")
        destroy_block(block, PAGESIZE)
        del block
        del mul

    if __name__ == "__main__":
        main()

It lets you call everything from the command line, supplying the constant to
multiply with. It contains a small loop to test that the code works. Running
it, we get

		$ python mj.py 11
		Pagesize: 4096
		Allocating one page of memory
		JIT-compiling a native mul-function w/arg 11
		Making function block executable
		Testing function
		mul(0) = 0
		mul(1) = 11
		mul(2) = 22
		mul(3) = 33
		mul(4) = 44
		mul(5) = 55
		mul(6) = 66
		mul(7) = 77
		mul(8) = 88
		mul(9) = 99
		Deallocating function

Closing remarks
---------------

Let me know if anything is unclear. I admit I wrote this quickly, so some parts
may be quite obscure if you're just starting out.

Before you start writing huge applications using this, check out the
[PeachPy][peachpy] project. It goes way beyond this and includes a disassembler
and supports seemingly the entire x86-64 instruction set right up to AVX.

Finally, don't expect small JIT-ed functions to perform well in Python. There
is quite some overhead involved with ctypes. I haven't looked into the details
of ctypes, but if it does use libffi, there is indeed a lot of overhead.

If you want to continue writing things yourself, you may soon need a
disassembler. While you can use gdb to break into Python and disassemble your
JIT-ed code, it's probably better to use a Python package for that. For
example [Capstone][capstone].

[capstone]: http://www.capstone-engine.org/lang_python.html
[cffi.github]: https://github.com/cffi/cffi
[ctypes.doc]: https://docs.python.org/3/library/ctypes.html#module-ctypes
[github]: https://github.com/cslarsen/minijit
[mmap.man]: http://man7.org/linux/man-pages/man2/mmap.2.html
[mprotect.man]: http://man7.org/linux/man-pages/man2/mprotect.2.html
[peachpy]: https://github.com/Maratyszcza/PeachPy
[wx.wiki]: https://en.wikipedia.org/wiki/W%5EX
