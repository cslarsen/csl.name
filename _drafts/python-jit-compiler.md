---
layout: post
title: "A basic x86-64 JIT compiler in Python"
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
built-in module [`ctypes`][ctypes.doc]. An FFI module like
[`cffi`][cffi.github] would make things a bit easier, but it's unfortunately
not yet part of the standard distribution. Using Python means that we actually
have to delve very close to the OS, meaning that we'll be exposed to its C
interface.

Our strategy is simply to grab a piece of memory, write machine code to it,
mark it as executable and then jump to it. We will use the [`mmap`][mmap.man] C
function to fetch memory and [`mprotect`][mprotect.man] to change its execution
bit.

Preliminary step
----------------

Before we can do anything, we need to load the standard C library.

    if sys.platform.startswith("darwin"):
        libc = ctypes.cdll.LoadLibrary("libc.dylib")
    elif sys.platform.startswith("linux"):
        libc = ctypes.cdll.LoadLibrary("libc.so.6")

To change the execution bit on a block of memory, the operating system requires
that it is page-aligned. Because of that, we need to know the pagesize, so we
can allocate exact multiples of the pagesize. We can do this with the `sysconf`
function with `_SC_PAGESIZE` as argument.

`sys/mman.h` C header file, and its value is different across operating
But what is the integral value of `_SC_PAGESIZE` ? This is defined in the
define a few enumeration values that we are going to use.
is able to parse C header files, while ctypes is not. Nevertheless, we need to
systems. This is one of the spots where the [cffi][cffi.github] module makes life easier: It

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

We can now proceed to set up the signatures for the C functions we will call:

    # Set up sysconf
    sysconf = libc.sysconf
    sysconf.argtypes = [ctypes.c_int]
    sysconf.restype = ctypes.c_long

    # Get pagesize
    PAGESIZE = sysconf(_SC_PAGESIZE)

Our block of code will be interpreted as unsigned 8-bit bytes, so declare a
pointer type for it:

    # 8-bit unsigned pointer type
    c_uint8_p = ctypes.POINTER(ctypes.c_uint8)

Finally, we just dish out the signatures from the rest of the functions that we
will use. These can be seen from the manual pages for `mmap`, `mprotect` and
`strerror`.

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
justify writing this in Python rather than pure C. But down the line, it will
enable us to experiment much more easily with JIT-compilation with the
simplicity and fluidity of Python. Whatever.

Helper functions
----------------

Now we're ready to write the `mmap` wrapper.

    def create_block(size):
        ptr = mmap(0, size, PROT_WRITE | PROT_READ,
                MAP_PRIVATE | MAP_ANONYMOUS, 0, 0)

        if ptr == MAP_FAILED:
            raise RuntimeError(strerror(ctypes.get_errno()))

        return ptr

To mark memory as executable,

    def make_executable(block, size):
        if mprotect(block, size, PROT_READ | PROT_EXEC) != 0:
            raise RuntimeError(strerror(ctypes.get_errno()))

and to destroy a block of code

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

    int multiply(int n)
    {
      return n*113;
    }

Compile with something like

    $ gcc -O3 multiply.c -fPIC -shared -olibmultiply.so

Then dump the machine code

    $ objdump -d libmultiply.so 

    libmultiply.so:     file format mach-o-x86-64

    ...

    0000000000000fa0 <_multiply>:
     fa0:	55                   	push   %rbp
     fa1:	48 89 e5             	mov    %rsp,%rbp
     fa4:	6b c7 71             	imul   $0x71,%edi,%eax
     fa7:	5d                   	pop    %rbp
     fa8:	c3                   	retq   

    ...

We'll use a slightly different bit of machine code.

    def make_multiplier(block, multiplier):
      # Prologue

      # push rbp
      block[0] = 0x55

      # mov rbp, rsp
      block[1] = 0x48
      block[2] = 0x89
      block[3] = 0xe5

      # put argument onto stack.. :)
      block[4] = 0x89
      block[5] = 0x7d
      block[6] = 0xfc

      # get argument into eax :D mov eax, dword ptr [rbp-0x4]
      block[7] = 0x8b
      block[8] = 0x45
      block[9] = 0xfc

      # mov edx, immediate 32-bit value
      block[10] = 0xba

      # little-endian
      block[11] = (multiplier & 0x000000ff)
      block[12] = (multiplier & 0x0000ff00) >> 8
      block[13] = (multiplier & 0x00ff0000) >> (4*4)
      block[14] = (multiplier & 0xff000000) >> (6*4)

      # imul eax, edx
      block[15] = 0x0f
      block[16] = 0xaf
      block[17] = 0xc2

      # Epilogue: pop rbp
      block[18] = 0x5d

      # retq
      block[19] = 0xc3

      # Make a function out of this

      function = ctypes.CFUNCTYPE(ctypes.c_int)
      function.restype = ctypes.c_int
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
