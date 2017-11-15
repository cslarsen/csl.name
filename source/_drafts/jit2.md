---
layout: post
title: "JIT-compiling a tiny subset of Python to native x86-64 code"
date: 2017-11-14 17:50:12 +0100
updated: 2017-11-14 17:50:12 +0100
categories: Python assembly
disqus: true
tags: Python assembly
---

In this post I'll show how to JIT-compile a tiny subset of Python into native
x86-64 machine code. We will build directly on the techniques established in
[Writing a basic x86-64 JIT compiler from scratch in stock
Python][previous-post]. As before, we will restrict ourselves to using only
built-in CPython modules. The code in this post is available at
[github.com/cslarsen/minijit][minijit.github].

Our goal is to enable compilation of Python functions to native code, like
this:

    >>> def foo(a, b):
    ...   return a*a - b*b
    ...
    >>> bar = compile_native(foo)
    >>> bar(1, 2)
    -3

To keep the scope manageable, we'll restrict ourselves to compiling branchless
functions — functions without if-statements, loops or function calls — that
operate on integer arithmetic.

Our strategy is to translate Python bytecode into a simple [intermediate
representation][ir.wiki], perform obvious [peep-hole optimizations] on it and
translate that directly to native x86-64 machine code. We'll leverage the code
in the [previous post][previous-post] to bind the machine code to a callable
Python function. But first we need to understand how Python bytecode works.

Part one: How Python bytecode works
-----------------------------------

You can see the raw bytecode for the `foo` function at the top in Python 3 by
typing

    >>> foo.__code__.co_code
    b'|\x00|\x00\x14\x00|\x01|\x01\x14\x00\x18\x00S\x00'

In Python 2.7, it would be

    >>> foo.func_code.co_code
    '|\x00\x00|\x00\x00\x14|\x01\x00|\x01\x00\x14\x18S'

Because the two bytecodes are nearly identical, it doesn't matter which one
will be used to explain how they work. I've picked Python 2.7 for this post,
but the [GitHub repository][minijit.github] supports both.

Let's have a look at the disassembly.

    >>> import dis
		>>> dis.dis(foo)
			2           0 LOAD_FAST                0 (a)
									3 LOAD_FAST                0 (a)
									6 BINARY_MULTIPLY
									7 LOAD_FAST                1 (b)
								 10 LOAD_FAST                1 (b)
								 13 BINARY_MULTIPLY
								 14 BINARY_SUBTRACT
								 15 RETURN_VALUE

The leftmost `2` is the source code line number. The numbers in the next column
are the bytecode offsets: We can clearly see that the `LOAD_FAST` instruction
takes three bytes. The first one is the instruction identifier, followed by a
16-bit argument. For the `LOAD_FAST` instruction, the argument is an index into
a list of local variables. The variable `a` is obviously first in this list.
The `LOAD_FAST` instructions at offsets 7 and 10 refer to the second variable
`b`.

Just like the JVM, CLR, Forth and many other successful languages, CPython is
implemented as a [stack machine][stack-machine].


[constant-folding]: https://en.wikipedia.org/wiki/Constant_folding
[cpython-eval]: https://github.com/python/cpython/blob/1896793/Python/ceval.c#L1055
[github]: https://github.com/cslarsen/minijit
[hn.front]: https://news.ycombinator.com/front?day=2017-11-09
[hn]: https://news.ycombinator.com/item?id=15665581
[minijit.github]: https://github.com/cslarsen/minijit
[mj.github]: https://github.com/cslarsen/minijit
[nasm]: http://www.nasm.us
[previous-post]: /post/python-jit/
[registers.wiki]: https://en.wikipedia.org/wiki/Processor_register
[stack-machine]: https://en.wikipedia.org/wiki/Stack_machine
[stack-register.wiki]: https://en.wikipedia.org/wiki/Stack_register
