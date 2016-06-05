---
layout: post
title: "How GCC fixes bad hand-optimizations"
date: 2016-06-05 01:00:24 +0200
updated: 2016-06-05 01:00:24 +0200
categories: programming
disqus: true
tags: c++ llvm optimization assembly
---

The GCC and LLVM optimizers contains a <a
href="http://www.fefe.de/source-code-optimization.pdf">trove of arcane and
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
faster instructions. Noticing that 320 = 2<sup>6</sup> + 2<sup>8</sup>, the
above expression can be optimized to

    size_t offset = x + (y << 6) + (y << 8);

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

Compile that without optimizations, 

    $ llvm-gcc -m64 -march=native -mtune=native -O0 -S foo.c

and look at its assembly. First it loads `x` and `y` into `esi` and `edi`,
respectively:

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

It still does add and shifts, but in a different way. The above code first does

    eax = y + y*4
    eax = eax << 6
    eax = eax + x

or

    return x + ((y+y*4) <<6 )

It does the *exact* same for the straight-forward version,

    unsigned offset(unsigned x, unsigned y)
    {
      return x + y*320;
    }

For 32-bit targets, the assembly will be structurally equivalent.

But what about GCC? It goes even further! Compiling the original shift-and-add
code with

    $ gcc-5 -mtune=native -march=native -m64 -Ofast -S foo.c

produces

    imull $320, %esi, %eax
    addl  %edi, %eax
    ret

Simply `return x + y*320`.

So which one is faster? According to some an <a
href="https://gist.github.com/cslarsen/2896137">old experiment I made</a>,
the GCC version is the fastest — **but** I'll need to rerun them and compile
from assembly, to make sure that the optimizer doesn't optimize away the loop.
I'll post results at a later time.

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
