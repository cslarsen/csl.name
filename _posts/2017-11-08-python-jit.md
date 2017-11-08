---
layout: post
title: "Writing a basic x86-64 JIT compiler from scratch in stock Python"
date: 2017-11-08 22:03:00 +0100
updated: 2017-11-08 22:03:00 +0100
categories: Python assembly
disqus: true
tags: Python assembly
---

In this post I'll show how to write a rudimentary, native x86-64 [just-in-time
compiler (JIT)][jit.wiki] in CPython, using only the built-in modules.

The code here specifically targets the UNIX systems macOS and Linux, but should
be easily translated to other systems such as Windows. The complete code is
available at [https://github.com/cslarsen/minijit][github].

The goal is to generate new versions of the below assembly code at runtime and
execute it.

    48 b8 ed ef be ad de  movabs $0xdeadbeefed, %rax
    00 00 00
    48 0f af c7           imul   %rdi,%rax
    c3                    retq

We will mainly deal with the left hand side — the byte sequence `48 b8 ed ...`
and so on.  Those fifteen machine code bytes comprise an x86-64 function that
multiplies its argument with the constant [`0xdeadbeefed`][deadbeef]. The JIT
step will create functions with different such constants.  While being a
contrived form of [specialization][specialization.wiki], it will illuminate the
basic mechanics of just-in-time compilation.

Our general strategy is to rely on the built-in [`ctypes`][ctypes.doc] module
to load the C standard library. From there, we can access system functions to
interface with the virtual memory manager. We'll use [`mmap`][mmap.man] to fetch a
page-aligned block of memory. To be on the safe side, we'll fetch exactly one
page of memory. The alignment requirement is why we can't simply use `malloc`,
because we never know if it returns memory that spans page boundaries.

The function [`mprotect`][mprotect.man] can be used to mark the memory block as
read-only and executable. From there, we can call into the block through
ctypes.

The boiler-plate part
---------------------

Before we can do anything, we need to load the standard C library.

    import ctypes

    if sys.platform.startswith("darwin"):
        libc = ctypes.cdll.LoadLibrary("libc.dylib")
        # ...
    elif sys.platform.startswith("linux"):
        libc = ctypes.cdll.LoadLibrary("libc.so.6")
        # ...

To find the page size, we'll call `sysconf(_SC_PAGESIZE)`. The `_SC_PAGESIZE`
constant is 29 on macOS and 30 on Linux. We'll just hard-code those in our
program. You can find those numbers by digging into system header files
or by writing a simple C program that prints them out. A more robust and
elegant solution would be to use the [`cffi` module][cffi.github] instead of
ctypes. It can automatically parse header files. However, I wanted to stick to
the default Python distribution, so we'll stick to ctypes in this post.

We need a few additional constants for `mmap` and `mprotect`. I've just written
them out below. Note that if you want to run this code on other UNIX
distributions, you may have to look up the constants again (for all I know,
they may even change between kernel versions).

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
types. For example

    # Set up sysconf
    sysconf = libc.sysconf
    sysconf.argtypes = [ctypes.c_int]
    sysconf.restype = ctypes.c_long

tells ctypes that `sysconf` is a function that takes a single integer and
produces a long integer. After this, we can get the current page size with

    PAGESIZE = sysconf(_SC_PAGESIZE)

The machine code we are going to generate will be interpreted as unsigned 8-bit
bytes, so we need to declare a new pointer type:

    # 8-bit unsigned pointer type
    c_uint8_p = ctypes.POINTER(ctypes.c_uint8)

Below we just dish out the remaining signatures for the functions that we'll
use. For error reporting, it's good to have the [`strerror`][strerror.man]
function available.  We'll use [`munmap`][munmap.man] to destroy the machine
code block after we're done with it.  It lets the operating system reclaim that
memory.

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

At this point, it's hard to justify using Python rather than C. With C, we
don't need any of the above boiler-plate code. But down the line, Python will
allow us to experiment much more easily with JIT
With all the boiler-plate code above, I have to admit it
At this point, with all the boiler-plate code, I have to admit it's hard to
justify writing this in Python rather than pure C. We're working with a C
library at the ABI-level, which can be quite flaky. Also, our code is larger
than an equivalent C code up to this point. But down the line, Python will make
it vastly simpler to experiment with JIT-compilation.

Now we're ready to write the `mmap` wrapper.

    def create_block(size):
        ptr = mmap(0, size, PROT_WRITE | PROT_READ,
                MAP_PRIVATE | MAP_ANONYMOUS, 0, 0)

        if ptr == MAP_FAILED:
            raise RuntimeError(strerror(ctypes.get_errno()))

        return ptr

This function uses `mmap` to allocate page-aligned memory for us. We mark the
memory region as readable and writable with the PROT flags, and we also mark it
as private and anonymous. The [`mmap` manual page][mmap.man] covers the
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

The fun part
------------

We're now ready to create an insanely simple piece of JIT code. Recall the
assembly listing at the top: It's a small function — without a local stack
frame — that multiplies an input number with a constant. In Python, we'd write
that as

    def create_multiplication_function(constant):
        return lambda n: n * constant

This is indeed a very contrived example, but qualifies a JIT. After all, we do
create native code at runtime and execute it. It's easy to imagine more
advanced examples such as JIT-compiling [Brainfuck][brainfuck.wiki] to x86-64
machine code. Or using [AVX][avx.wiki] instructions for blazing fast,
vectorized math ops.

The disassembly at the top of this post generated by compiling the following
piece of code and disassembling it:

    #include <stdint.h>

    uint64_t multiply(uint64_t n)
    {
      return n*0xdeadbeefedULL;
    }

I then compiled it to a shared library using gcc.

    $ gcc -Os -fPIC -shared -fomit-frame-pointer \
        -march=native multiply.c -olibmultiply.so

Here I optimized for space (`-Os`) to generate as little machine code as
possible, with position-independent code (`-fPIC`) to prevent using absolute
jumps, without any call frame setup (`-fomit-frame-pointer`) so we won't get
any superfluous stack setup (but that will be required for more advanced
functions) and using the current CPU's instruction set (`-march=native`).

We could also have passed `-S` to gcc to produce a disassembly listing, but
we're actually interested in the machine code, so better to use a tool like
`objdump`:

    $ objdump -d libmultiply.so
    ...
    0000000000000f71 <_multiply>:
     f71:	48 b8 ed ef be ad de 	movabs $0xdeadbeefed,%rax
     f78:	00 00 00 
     f7b:	48 0f af c7          	imul   %rdi,%rax
     f7f:	c3                   	retq

In case you are not familiar with assembly, I'll let you know how this function
works. First, the `movabs` function just puts an _immediate_ number in the RAX
register. _Immediate_ is assembly-jargon for encoding something right in the
machine code. In other words, it's an embedded argument for the `movabs`
instruction. So RAX now holds the constant `0xdeadbeefed` (the constant is a
bland play on one of the MACH file format magic numbers on macOS systems).

called passed in. Also, it is the convention that return values are passed in
first argument of a function is placed in RDI. So RDI will hold the number the
Next, the `imul` instruction multiplies the numbers in the RDI and RAX
registers, and places the result in RAX. Per the [AMD64 ABI][amd64.abi] the
the RAX register, so we can just return from the function with `retq`.

RETQ means that it will pop the current 64-bit value on the stack and jump to
it. It is normal to use the CALLQ instruction to call subroutines, and CALLQ
will push the current 64-bit address on the stack before jumping. So at this
last point in our code, it will — most likely — contain the return address of
the caller. Of course, assembly is all about convention, so it you write an
entire program yourself, you could put anything you'd like there. Perhaps
you're using continuation-passing style, whatever.

The last thing to note is that the the constant `0xdeadbeefed` is encoded in
little-endian format. So that means, as we patch up this piece of code, we need
to encode our constant the same way. (A good mnemonic to remember the order is
that little endian means "little-end first", meaning the smallest part of the
number comes first as you traverse memory).  The whole operation we're doing is
actually a simple form of [specialization][specialization.wiki].

We are now ready to put everything in a Python function.

    def make_multiplier(block, multiplier):
        # Encoding of: movabs <multiplier>, rax
        block[0] = 0x48
        block[1] = 0xb8

        # Little-endian encoding of multiplication constant
        block[2] = (multiplier & 0x00000000000000ff) >>  0
        block[3] = (multiplier & 0x000000000000ff00) >>  8
        block[4] = (multiplier & 0x0000000000ff0000) >> 16
        block[5] = (multiplier & 0x00000000ff000000) >> 24
        block[6] = (multiplier & 0x000000ff00000000) >> 32
        block[7] = (multiplier & 0x0000ff0000000000) >> 40
        block[8] = (multiplier & 0x00ff000000000000) >> 48
        block[9] = (multiplier & 0xff00000000000000) >> 56

        # Encoding of: imul rdi, rax
        block[10] = 0x48
        block[11] = 0x0f
        block[12] = 0xaf
        block[13] = 0xc7

        # Encoding of: retq
        block[14] = 0xc3

        # Return a ctypes function with the right prototype
        function = ctypes.CFUNCTYPE(ctypes.c_int64)
        function.restype = ctypes.c_int64
        return function

In the final part of the function, we create a ctypes function signature that
can be used to call this function. It's somewhat arbitrarily placed, but I
thought it was good to keep the signature close to the machine code.

The final part
--------------

Now that we have the basic parts we can weave everything together. The first
part is to allocate one page of memory:

    pagesize = sysconf(_SC_PAGESIZE)
    block = create_block(pagesize)

Next, we generate the machine code. Let's pick the number 101 to use as a
multiplier.

    mul101_signature = make_multiplier(block, 101)

We now mark the memory region as executable and read-only:

    make_executable(block, pagesize)

Take the address of the first byte in the memory block and cast it to a
callable ctypes function with proper signature:

    address = ctypes.cast(block, ctypes.c_void_p).value
    mul101 = mul101_signature(address)

To get the memory address of the block, we use ctypes to cast it to a void
pointer and extract its value. Finally, we instantiate an actual function from
this address using the `mul101_signature` constructor.

Voila! We now have a piece of _native_ code that we can call from Python. If
you're in a REPL, you can try it directly:

    >>> print(mul101(8))
    808

Note that this small multiplication function will run slower than a native
Python calculation. That's mainly because ctypes, being a foreign-function
library, has a lot of overhead: It needs to inspect what dynamic types you pass
the function every time you call it, then unbox them, convert them and then do
the same with the return value. So the trick is to either use assembly because
you have to access some new Intel instruction, or because you compile something
like Brainfuck to native code.

Finally, if you want to, you can let the system reclaim the memory holding the
function. Beware that after this, you will probably crash the process if you
try calling the code again. So probably best to delete all references in
Python as well:

    destroy_block(block, pagesize)

    del block
    del mul101

If you run the code in its complete form from the [GitHub][github] repository,
you can run the `mj.py` program and put the multiplication constant on the
command line:

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

Debugging JIT-code
------------------

If you want to continue learning with this simple program, you'll quickly want
to disassemble the machine code you generate. One option is to simply use gdb
or lldb, but you need to know where to break. One trick is to just print the
hex value of the `block` address and then wait for a keystroke:

    print("address: 0x%x" % address)
    print("Press enter to continue")
    raw_input()

Then you just run the program in the debugger, break into the debugger while
the program is pausing, and disassemble the memory location. Of course you can
also step-debug through the assembly code if you want to see what's going on.
Here's an example lldb session:

    $ lldb python
    ...
    (lldb) run mj.py 101
    ...
    (lldb) c
    Process 19329 resuming
    ...
    address 0x1002fd000
    Press ENTER to continue

At this point, hit CTRL+C to break back into the debugger, then disassemble
from the memory location:

    (lldb) x/3i 0x1002fd000
        0x1002fd000: 48 b8 65 00 00 00 00 00 00 00  movabsq $0x65, %rax
        0x1002fd00a: 48 0f af c7                    imulq  %rdi, %rax
        0x1002fd00e: c3                             retq

Notice that 65 hex is 101 in decimal, which was the command line argument we
passed above.

If you're not interested in debugging, but just want a disassembler, I
recommend the [Capstone][capstone] module.

What's next?
------------

A good exercise would be to JIT-compile [Brainfuck programs][brainfuck.wiki] to
native code. If you want to jump right in, I've made a GitHub repository at
[https://github.com/cslarsen/brainfuck-jit][brainfuck.github]. I even have a
[Speaker Deck presentation][speakerdeck] to go with it. It performs JIT-ing and
optimizations, but uses GNU Lightning to compile native code instead of this
approach. It should be extremely simple to boot out GNU Lightning in favor or
some code generation of your own. An interesting note on the Brainfuck project
is that if you just JIT-compile each Brainfuck instruction one-by-one, you
won't get much of a speed boost, even if you run native code. The entire speed
boost is done in the _code optimization_ stage, where you can bulk up integer
operations into one or a few x86 instructions.

Also, before you get serious about expanding this JIT-compiler, take a look at
the [Peachpy project][peachpy]. It goes way beyond this and includes a
disassembler and supports seemingly the entire x86-64 instruction set right up
to [AVX][avx.wiki].

Closing remarks
---------------

As mentioned, there is a good deal of overhad when using ctypes to call into
functions. You can use the `cffi` module to overcome some of this, but the fact
remains that if you want to call very small JIT-ed functions a large number of
times, it's usually faster to just use pure Python.

What I think is fun about this exercise is to get into deeper territory than
pure assembly. One thing that comes to mind is how different instructions are
disassembled to the same mnemonic. For example, the RETQ instruction has a
different opcode than an ordinary RET, because it operates on 64-bit values.
This is something that may not be important when doing assembly programming,
because it's a detail that may not always matter, but it's worth being aware of
the difference. I saw that gcc, lldb and objdump gave slightly different
disassembly listings of the same code for RETQ and MOVABSQ.

[amd64.abi]: https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf
[avx.wiki]: https://en.wikipedia.org/wiki/Advanced_Vector_Extensions
[brainfuck.github]: https://github.com/cslarsen/brainfuck-jit
[brainfuck.wiki]: https://en.wikipedia.org/wiki/Brainfuck
[capstone]: http://www.capstone-engine.org/lang_python.html
[cffi.github]: https://github.com/cffi/cffi
[ctypes.doc]: https://docs.python.org/3/library/ctypes.html#module-ctypes
[deadbeef]: https://en.wikipedia.org/wiki/Magic_number_(programming)
[github]: https://github.com/cslarsen/minijit
[jit.wiki]: https://en.wikipedia.org/wiki/Just-in-time_compilation
[mmap.man]: http://man7.org/linux/man-pages/man2/mmap.2.html
[mprotect.man]: http://man7.org/linux/man-pages/man2/mprotect.2.html
[munmap.man]: http://man7.org/linux/man-pages/man3/munmap.3p.html
[peachpy]: https://github.com/Maratyszcza/PeachPy
[speakerdeck]: https://speakerdeck.com/csl/how-to-make-a-simple-virtual-machine
[specialization.wiki]: https://en.wikipedia.org/wiki/Run-time_algorithm_specialisation
[strerror.man]: http://man7.org/linux/man-pages/man3/strerror.3.html
[wx.wiki]: https://en.wikipedia.org/wiki/W%5EX
