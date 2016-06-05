---
layout: post
title: "How GCC fixes bad hand-optimizations"
date: 2016-06-05 01:00:24 +0200
updated: 2016-06-05 23:05:43 +0200
categories: programming
disqus: true
tags: c++ llvm optimization assembly gcc c
---

The GCC and LLVM optimizers contain <a
href="http://www.fefe.de/source-code-optimization.pdf">troves of arcane and
esoteric tricks</a> to speed up code on your particular system. Rather
surprisingly, it will even transform sub-optimal, "clever" code that does more
harm than good.

One such piece of code is using shifts and additions in place of
multiplications. In the olden days, this was a reliable way to speed up your
code, especially for game and demo programming. It's even mentioned in <a
href="http://www.fefe.de/source-code-optimization.pdf">Hacker's Delight</a>
(which I highly recommend for bit fiddlers).

To draw a pixel at given x and y positions, you must calculate the screen
offset:

    size_t offset = (x + y*width) * component_size;

Here, `component_size` is the size of each pixel. For 8-bit RGBA values, it
will be four bytes. In the nineties, graphics modes used color palettes, so
the component would be a one-byte index. <a
href="https://en.wikipedia.org/wiki/Mode_13h">Mode 13h</a> was 320 by 200
pixels, and often used by games and demos. This gives

    size_t offset = x + y*320;

However, the multiplication was quite expensive in those days, and with many
*individual* pixels being drawn, an optimization would be to replace it with
instructions taking less cycles. Noticing that 320 = 2<sup>6</sup> +
2<sup>8</sup>, the above expression can be transformed to

    size_t offset = x + (y << 6) + (y << 8);

We've replaced a multiplication and add with two shifts and two adds.

This was a tried-and-true technique, part of every programmer's bag of tricks.
I typed it out by reflex <a
href="https://news.ycombinator.com/item?id=4083414">for years</a>. However, on
modern CPUs in plain, non-vectorized code, those shifts and additions are
somewhat modified. The cool thing is that GCC picks up those false
optimizations and fixes it for you. Consider

    unsigned offset(unsigned x, unsigned y)
    {
      return x + (y << 6) + (y << 8);
    }

Compile that with LLVM without optimizations,

    $ llvm-gcc -m64 -march=native -mtune=native -O0 -S foo.c

and look at its assembly. This was done on an i7 on OS X.  First it loads `x`
and `y` into `esi` and `edi`, respectively:

    movl    -4(%rbp), %esi
    movl    -8(%rbp), %edi

It then performs `x += (y << 6)`

    shll    $6, %edi
    addl    %edi, %esi

and then `x += (y << 8)`

    movl    -8(%rbp), %edi
    shll    $8, %edi
    addl    %edi, %esi

Turning on optimizations,

    $ llvm-gcc -m64 -march=native -mtune=native -O3 -S foo.c

it will change slightly to

    leal    (%rsi,%rsi,4), %eax
    shll    $6, %eax
    addl    %edi, %eax

It still does add and shifts, but in a different way. Excluding the function
prologue and epilogue, the above code does

    eax = y + y*4
    eax = eax << 6
    eax = eax + x

or

    return x + ((y+y*4) <<6 )

LLVM will do the *exact* same for the straight-forward version,

    unsigned offset(unsigned x, unsigned y)
    {
      return x + y*320;
    }

and 32-bit targets are structurally equivalent.  But what about **GCC**? It
goes even further!

Compiling the original shift-and-add code with

    $ gcc-5 -mtune=native -march=native -m64 -Ofast -S foo.c

produces

    imull $320, %esi, %eax
    addl  %edi, %eax
    ret

It's simply `return x + y*320`. But is it faster? Yes, it is!

Testing performance
--------------------

When it comes to performance testing, there's nothing like actually running
real code. So let's put the functions to the test.

We'll use <a href="http://www.nasm.us">NASM</a> to assemble the two versions
above, and GCC 5 to compile a test driver. We'll turn off optimizations where
needed.

The assembly code for the imul and shift + add functions are placed in
`offset.asm`. I'm on OSX right now, so we'll need to use the correct alignment,
otherwise the linker won't be able to find the functions. The file looks like
this:

    ; OSX 64-bit, requires special alignment
    bits 64
    align 16

    ; For the symbol table
    global _offset_imul
    global _offset_shift_add

    section .text

    ; Takes x and y, returns x + y*320
    _offset_imul:
      imul eax, esi, 320
      add eax, edi
      ret

    ; The same, but with shifts and adds
    _offset_shift_add:
      lea eax, [rsi, rsi*4]
      shl eax, 6
      add eax, edi
      ret

Compile this with

    $ nasm -fmacho64 offset.asm -ooffset.o

The test driver program, called `test-offset.c`, uses <a
href="https://developer.apple.com/library/ios/documentation/System/Conceptual/ManPages_iPhoneOS/man2/getrusage.2.html">getrusage</a>
to find the CPU time spent by each function. To compare the performance of each
function, we'll run one billion iterations of them each, then keep the *best*
time so far. That's the approach Facebook uses in their profiling code in <a
href="https://github.com/facebook/folly/blob/master/folly/Benchmark.cpp#L171">Folly</a> (apparently they went back to this after testing some statistical modelling).
For code like this, we don't want to measure nonsense things like average
running time and so on: The best run gives the most correct measurement of how
well the function performs.

    #include <assert.h>
    #include <stdio.h>
    #include <sys/resource.h>

    static unsigned correct_result = 0;
    typedef unsigned (*func_t)(unsigned, unsigned);

    /* Our assembly functions */
    extern unsigned __attribute__((optimize("O0")))
      offset_imul(unsigned x, unsigned y);
    extern unsigned __attribute__((optimize("O0")))
      offset_shift_add(unsigned x, unsigned y);

    /* To make sure that the functions actually work */
    static unsigned correct(const unsigned x, const unsigned y)
    {
      return x + y*320;
    }

    static double rusage()
    {
      struct rusage ru;
      getrusage(RUSAGE_SELF, &ru);
      return ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1000000.0;
    }

    /* We'll disable optimizations for this one */
    static unsigned __attribute__((optimize("O0")))
      calc(const size_t its, func_t func)
    {
      unsigned check = 0xaaaaaaaa;

      for ( size_t n=0; n<its; ++n )
        check ^= func(n, n+1);

      return check;
    }

    static void
      __attribute__((optimize("O0")))
      best(const size_t its, double* best, func_t func)
    {
      const double start = rusage();
      const unsigned result = calc(its, func);
      assert(result == correct_result);
      const double secs = rusage() - start;

      if ( secs < *best )
        *best = secs;
    }

    int main()
    {
      double tc=9999, ti=9999, ts=9999;

      const size_t its = 1000000000;
      correct_result = calc(its, correct);

      /* Loop forever and print the running best results */
      for (;;) {
        best(its, &tc, correct);
        best(its, &ts, offset_shift_add);
        best(its, &ti, offset_imul);
        printf("correct %fs, shift+add %fs, imul %fs\n", tc, ts, ti);
      }
    }

Compile this with

    $ gcc-5 -W -Wall -Ofast offset.o test-offset.c -otest-offset

Results
-------

We're finally ready to party (according to some definition of "party"):

    $ ./test-offset
    correct 3.448534s, shift+add 3.301471s, imul 3.164973s
    correct 3.304378s, shift+add 3.281795s, imul 3.161455s
    correct 3.285387s, shift+add 3.254211s, imul 3.161455s
    ...
    correct 3.267140s, shift+add 3.254211s, imul 3.161455s

Clearly, the `imul` function is the fastest. But only by a small amount.
However, the point here was to show how GCC will fix bad hand-optimizations.
And clearly, it did that correctly!

What's interesting is that GCC 5 obviously didn't optimize the `correct`
function as well as either of `offset_imul` of `offset_shift_add` in this
program. You can take a look at the disassembly by doing `objdump -d
test-offset` and see for yourself. 

Obviously, the first mistake for the
`correct` function is that it creates a stack frame. If you recompile with
`-fomit-frame-pointer`, GCC quickly rectifies that, and we get the shift + add
version (but *not* the imul one!):

    0000000100000c00 <_correct>:
       100000c00: 8d 04 b6              lea    (%rsi,%rsi,4),%eax
       100000c03: c1 e0 06              shl    $0x6,%eax
       100000c06: 01 f8                 add    %edi,%eax
       100000c08: c3                    retq
       100000c09: 0f 1f 80 00 00 00 00  nopl   0x0(%rax)

Rerunning the tests, I got these results:

correct 3.357759s, shift+add 3.311668s, imul 3.159475s
correct 3.228023s, shift+add 3.311668s, imul 3.159475s
...
correct 3.228023s, shift+add 3.244624s, imul 3.159475s

Note that `correct` and `shift+add` should actually get the exact same timings,
but they don't! That's most likely because of <a
href="https://en.wikipedia.org/wiki/Preemption_(computing)">preemption
noise</a> by the OS.  I didn't bother trying to run this in single user mode,
with `nice -10` and so on. If you try this out yourself, particularly if you're
on another OS, let me know your results in the comments!

Another question is why LLVM doesn't perform the same optimization. GCC is
supposedly somewhat faster than LLVM, but I haven't checked if this is still
true. Also, what we haven't looked at is any side-effects on the code. I'm
thinking about the cascading CPU pipeline and stuff like that. I don't expect
to see anything interesting here, though, because we're only accesing values on
the stack, and so on. Let me know in the comments if you have two cents to
spare!

I also tried adding `-m64 -march=native -mtune=native`, and the results I got
then was

correct 3.404448s, shift+add 3.271211s, imul 3.217215s
correct 3.269317s, shift+add 3.243117s, imul 3.173220s
correct 3.269317s, shift+add 3.243117s, imul 3.123939s
...

Each run is a little bit different, though.

Versions used
-------------

    $ gcc-5 --version
    gcc-5 (Homebrew gcc 5.3.0 --without-multilib --with-jit) 5.3.0
    Copyright (C) 2015 Free Software Foundation, Inc.
    This is free software; see the source for copying conditions.  There is NO
    warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    $ llvm-gcc --version
    Apple LLVM version 7.0.2 (clang-700.1.81)
    Target: x86_64-apple-darwin15.5.0
    Thread model: posix

    $ nasm -v
    NASM version 2.12.01 compiled on Mar 23 2016
