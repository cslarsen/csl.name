---
layout: post
title: "Embedding binary data in executables"
date: 2016-06-08 01:09:40 +0200
updated: 2016-06-08 01:09:40 +0200
categories: programming
disqus: true
tags: linker assembly nasm gcc c c++
---

Applications usually load resources like images from disk. But, in some
situations, it may be better to embed binary data right into the executable
file. Here are a few ways to do that.

In language like C and C++, the straight-forward way to include binary data in
the executable is to convert it to a character array:

    const char data[] = {0x00, 0x01, ...};

While there's nothing wrong with that approach, it requires a tool to convert
binary data to code (for example, `xxd -i`). Besides, I find it a bit
inelegant, and I'll present some alternatives for you.

Using the GNU linker
--------------------

This is by far the easiest solution — but, unfortunately, doesn't work on Mac
OS X. It does for Linux, though!

Let's say you have an image `cat.png` and want to embed it into your
application. You can create an object file with

    $ ld -r -b binary cat.png -o cat.o

The object file will have three symbols in it,

    $ nm cat.o
    _cat_start
    _cat_end
    _cat_size

(That's not actual output, but gives you an idea on how to see the symbols. You
can also use `objdump -x cat.o`).

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

If you don't have a working GNU `ld` — or, if you want utterly and complete
control — you can use an assembler like <a href="http://www.nasm.us">nasm</a>
to embed data.

Just embed the data with the `incbin` directive. Add some helper symbols for
the end of the data, and use a macro to calculate the byte length:

    bits 64

    section .rodata

    global _cat_start
    global _cat_end
    global _cat_size

    _cat_start:   incbin "cat.png"
    _cat_end:
    _cat_size:    dd $-_cat_start

Compile with `nasm -f<format> cat.asm -o cat.o`. On OS X, that's

    $ nasm -fmacho64 cat.asm -o cat.o

Finally, link your program exactly as before:

    $ gcc cat.o program.c -o program

So what's so good about this approach? It lets you put the binary data in the
read-only data section. It means that the data will be truly read-only.
If you used `ld -r -b binary` instead, you'd have to use `objcopy
--rename-section .data=.rodata,...` instead.

Using `objcopy`
---------------

You can also use `objcopy` from the GNU binutils package. However, I wasn't
able to get it working completely on OS X, and as it's not really a
cross-platform way to do it, I won't write much more about it.

But, you *can* start by doing

    $ objcopy -I binary -O mach-o-x86_64 \
      --rename-section .data=.const [...] cat.png cat.o

but the linker didn't like the resulting file very much. More on this later, if
I find a way. On Linux, it *should* be pretty straight forward.

What about other languages?
---------------------------

Most statically compiled languages will let you link in object files. I haven't
tried, but I guess you could easily do it in languages like Swift, Rust and so
on.

What would you use it for?
--------------------------

Of course, you can embed stuff like images and music into your application. For
desktop applications, this may be an advantage in certain situations. But there
are other cool uses as well.

Mike Pall, the original author of <a href="http://luajit.org">LuaJIT</a>, gives
an example where he <a
href="http://stackoverflow.com/a/11318414/21028">compiles Lua programs to
bytecode and wraps them up in an archive file</a>. He then proceeds to link a
host program in C with them, so the scripts can be executed without loading
anything from disk.

I'm sure there are many other use cases as well.
