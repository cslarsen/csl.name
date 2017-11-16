---
layout: post
title: "JIT-compiling a tiny subset of Python to native x86-64 code"
date: 2017-11-15 23:20:12 +0100
updated: 2017-11-15 23:20:12 +0100
categories: Python assembly
disqus: true
tags: Python assembly
---

In this post I'll show how to JIT-compile a tiny subset of Python into native
x86-64 machine code.

We will build directly on the techniques established in [<b>Writing a basic x86-64
JIT compiler from scratch in stock Python</b>][previous-post]. As before, we will
restrict ourselves to using only built-in CPython modules. The code in this
post is available at [github.com/cslarsen/minijit][minijit.github].

Our goal is to enable compilation of Python functions to native
code at runtime. I.e.,

    >>> def foo(a, b):
    ...   return a*a - b*b
    ...
    >>> bar = compile_native(foo)
    >>> bar(1, 2)
    -3

To keep the scope manageable, we'll restrict ourselves to compiling branchless
functions — that is, no if-statements, loops or function calls — that operate
purely on integer arithmetic.

Our strategy is to 

  * Translate Python bytecode into an [intermediate representation (IR)][ir.wiki]
  * Perform optimizations on the IR
  * Translate IR to native x86-64 machine code
  * Leverage code from [the previous post][previous-post] to bind the machine
    code to callable Python functions

The first part will then be to understand how the Python bytecode works.

Part one: How the Python bytecode works
---------------------------------------

You can see the raw bytecode for the `foo` function at the top in Python 3 by
typing

    >>> foo.__code__.co_code
    b'|\x00|\x00\x14\x00|\x01|\x01\x14\x00\x18\x00S\x00'

In Python 2.7, that would be

    >>> foo.func_code.co_code
    '|\x00\x00|\x00\x00\x14|\x01\x00|\x01\x00\x14\x18S'

Because the two bytecode sequences are near identical, it doesn't matter which
one will be used for the explanation. I've picked Python 2.7 here, but the
[GitHub code][minijit.github] supports both.

Let's have a look at the disassembly of `foo`.

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

The leftmost number `2` is the Python source code line number. The next column
contains the bytecode offsets.  We clearly see that the `LOAD_FAST` instruction
takes three bytes: One for the opcode (which instruction it is) and two for a
16-bit argument. That argument is zero, referring to the first argument `a`. 

CPython — like the JVM, CLR, Forth and many others – is implemented as a [stack
machine][stack-machine]. All the bytecode instructions operate on a _stack_ of
objects. For example, `LOAD_FAST` will _push_ a reference to the variable `a`
on the stack, while `BINARY_MULTIPLY` will pop off two, multiply them together
and put their product on the stack. (For our purposes, we will treat the stack
as holding integer _values_.)

A beautiful property of postfix systems is that
subexpressions can be serialized into a linear list of operations. For example,
in an infix mathematical expression such as

    2*2 - 3*3

we would need to compute the products _before_ subtracting, jumping back and
forth.  But in a _postfix_ system, we would only ever need to scan forwards.
For example, the above expression can be translated to
[Reverse Polish Notation (RPN)][rpn.wiki] using
the [Shunting-yard algorithm][shunting-yard.wiki]:

    2 2 * 3 3 * -

We now move from left to right: Push `2` and `2` on the stack. Pop two values
off the stack and push their product `4`. Push `3` and `3`, pop them off and
push their product `9`. The stack will now contain `9` on the top and `4` at
the bottom. We pop them off, subtract `9` from `4`, pushing the sole result
`-5` on the stack.

Now, imagine that the expression was actually written as

    sqrt(square(2) - square(3))

Like before, we could encode that in postfix form with

    push 2, square, push 3, square, subtract, sqrt

Notice how the use of a stack makes it possible to execute instructions
linearly. This is exactly how a stack machine operates. With that, you will
probably understand most of the [CPython opcodes][python.opcodes] and its
[interpreter loop][python.eval].

Part two: Translating Python bytecode to IR
-------------------------------------------

Our [intermediate representation (IR)][ir.wiki] will be naive. We'll blissfully forego
things like [three-address codes (TAC)][tac.wiki], [single-static assignment
(SSA)][ssa.wiki] and [register allocation][register-allocation.wiki].

Instead, our IR will be dead simple, consisting of pseudo-assembly instructions
that we can easily translate to machine code.  But that means we have to decide
now on how to implement things in machine code.

We will reserve the registers RDI, RSI, RDC and RCX for holding variables
and arguments. Per [AMD64 convention][amd64.abi], we expect to see function
arguments passed in those registers, in that order. When an instruction then
refers to variable number `n`, we can just look the register up in the tuple

    ARGUMENT_ORDER = ("rdi", "rsi", "rdx", "rcx")

This also means that we will support max four variables and arguments, since I
don't want the added complexity of dealing with stack frames in this post.

We reserve RAX and RBX to perform arithmetic. RAX also holds the return value.

The work registers for performing arithmetic will 
**The stack:** The CPU already has a stack, so we'll just use that to store
64-bit signed integer values. 

**Work registers:** We will reserve registers RAX and RBX as work registers for
arithmetic operations. Although RBX is conventionally required to be restored
by a functio before returning, we will simply ignore that. To be honest, I
haven't dug into the details of this, but the code does seem to work nicely to
thrash RBX. Perhaps `ctypes` pushes registers before calling into our function.

With that, we are ready to encode Python bytecode instructions in our IR.

We will implement a function `compile_ir` that takes Python bytecode along with
a list of constants. The constants can be found in Python with

    >>> def bar(n): return n*101
    ...
    >>> bar.func_code.co_consts
    (None, 101)

<table>
  <thead>
    <th>Instruction</th>
    <th>IR</th>
  </thead>
  <tbody>
    <tr>
      <td><code>LOAD_FAST n</code></td>
      <td><code>mov rax, ARGUMENT_ORDER[n]</code></td>
    </tr>
    <tr>
      <td></td>
      <td><code>push rax</code></td>
    </tr>
  </tbody>
</table>

    def compile_ir(bytecode, constants):
        # AMD64 argument passing order for our purposes.
        ARGUMENT_ORDER = ("rdi", "rsi", "rdx", "rcx")
        out = []

        while len(bytecode):
            opcode = dis.opname[bytecode.pop(0)]

            if opcode == "LOAD_FAST":
                index = bytecode.pop(0)
                if PRE36:
                    index |= bytecode.pop(0) << 8
                out.append(("push", ARGUMENT_ORDER[index]))

            # ...

            else:
                raise NotImplementedError(opcode)

        return out


[amd64.abi]: https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf
[constant-folding]: https://en.wikipedia.org/wiki/Constant_folding
[cpython-eval]: https://github.com/python/cpython/blob/1896793/Python/ceval.c#L1055
[github]: https://github.com/cslarsen/minijit
[hn.front]: https://news.ycombinator.com/front?day=2017-11-09
[hn]: https://news.ycombinator.com/item?id=15665581
[ir.wiki]: https://en.wikipedia.org/wiki/Intermediate_representation
[minijit.github]: https://github.com/cslarsen/minijit
[mj.github]: https://github.com/cslarsen/minijit
[nasm]: http://www.nasm.us
[previous-post]: /post/python-jit/
[python.eval]: https://github.com/python/cpython/blob/1896793/Python/ceval.c#L1055
[python.opcodes]: https://github.com/python/cpython/blob/master/Include/opcode.h
[register-allocation.wiki]: https://en.wikipedia.org/wiki/Register_allocation
[registers.wiki]: https://en.wikipedia.org/wiki/Processor_register
[rpn.wiki]: https://en.wikipedia.org/wiki/Reverse_Polish_notation
[shunting-yard.wiki]: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
[ssa.wiki]: https://en.wikipedia.org/wiki/Static_single_assignment_form
[stack-machine]: https://en.wikipedia.org/wiki/Stack_machine
[stack-register.wiki]: https://en.wikipedia.org/wiki/Stack_register
[tac.wiki]: https://en.wikipedia.org/wiki/Three-address_code
