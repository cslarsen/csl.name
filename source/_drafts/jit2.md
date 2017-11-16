---
layout: post
title: "JIT compiling a tiny subset of Python to x86-64"
date: 2017-11-15 23:20:12 +0100
updated: 2017-11-15 23:20:12 +0100
categories: Python assembly
disqus: true
tags: Python assembly
---

In this post I'll show how to JIT compile a tiny subset of Python into native
x86-64 machine code.

We will build directly on the techniques established in [<b>Writing a basic
x86-64 JIT compiler from scratch in stock Python</b>][previous-post], although
you can read this post on its own. As before, we will restrict ourselves to
using only built-in CPython modules. The code in this post is available at
[github.com/cslarsen/minijit][minijit.github].

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
16-bit argument. That argument is zero, referring to the first function
argument `a`. 

CPython — like the JVM, CLR, Forth and many others – is implemented as a [stack
machine][stack-machine]. All the bytecode instructions operate on a _stack_ of
objects. For example, `LOAD_FAST` will _push_ `a` reference to the variable `a`
on the stack, while `BINARY_MULTIPLY` will pop off two, multiply them together
and put their product on the stack. For our purposes, we will treat the stack
as holding _values_.

A beautiful property of postfix systems is that
operations can be serialized. For example, to compute an infix expression like

    2*2 - 3*3

we need to jump back and forth, calculating products before subtracting.
But in a _postfix_ system, we only need to scan forward.  For example, the
above expression can be translated to [Reverse Polish Notation (RPN)][rpn.wiki]
using the [Shunting-yard algorithm][shunting-yard.wiki]:

    2 2 * 3 3 * -

Moving from left to right, we push 2 on the stack, then push 2, pop the two
values off and put their product 4 on the stack. Push 3 and 3, pop them off and
push their product 9. The stack will now contain 9 on the top and 4 at the
bottom. For the final subtraction, we pop them off, perform the subtraction and
push the result -5 on the stack.

Now, imagine that the expression was actually written in a programming language
as

    subtract(multiply(2, 2), multiply(3, 3))

In postfix form, the order of evaluation becomes explicit:

    push 2, push 2, multiply, push 3, push 3, multiply, subtract

The use of a stack makes it possible to execute instructions linearly, and this
is essentially how stack machines operate. With that, you will probably
understand most of the [CPython opcodes][python.opcodes] and its [interpreter
loop][python.eval].

Part two: Translating Python bytecode to IR
-------------------------------------------

We will now translate each bytecode instructions to an [intermediate
representation (IR)][ir.wiki]. That is, in a form suitable for performing
things like analysis, translation and optimizations.  Ours will be blissfully
naive.  We will forego things like [three-address codes (TAC)][tac.wiki],
[single-static assignment (SSA)][ssa.wiki] and [register
allocation][register-allocation.wiki] for the sake of simplicity.

Our IR will consist of pseudo-assembly instructions in a list. For example

    ir = [("mov", "rax", 101),
          ("push", "rax", None)]

This is actually a form of [three-address codes (TAC)][tac.wiki], but we have
the operation first, then the destination and source registers. We put in
`None` to indicate unused arguments. So `sub rax, rbx` would be written in TAC
as `rax := rax - rbx`. It would also be a good idea to use abstract registers,
and always new ones, like `reg1`, `reg2` and so on. Then we could
[allocate][register-allocation.wiki] them actual CPU registers. We won't do
that here, though.

We will reserve registers RAX and RBX for things like pushing, popping and
arithmetic. RAX must also hold the return value, because that's the convention.
The CPU already has a stack, so we'll use that as our data stack mechanism.

Reigsters RDI, RSI, RDC and RCX will be reserved for variables and arguments.
Per [AMD64 convention][amd64.abi], we expect to see function arguments passed
in those registers, in that order.

Constants in the bytecode can be looked up with `co_consts`:

    >>> def bar(n): return n*101
    ...
    >>> bar.func_code.co_consts
    (None, 101)

We can now build a compiler that translates Python bytecode to our intermediate
representation. Its general form will be

    class Compiler(object):
        """Compiles Python bytecode to intermediate representation (IR)."""

        def __init__(self, bytecode, constants):
            self.bytecode = bytecode
            self.constants = constants
            self.index = 0

        def fetch(self):
            byte = self.bytecode[self.index]
            self.index += 1
            return byte

        def decode(self):
            opcode = self.fetch()
            opname = dis.opname[opcode]

            if opname.startswith(("UNARY", "BINARY", "INPLACE", "RETURN")):
                argument = None
            else:
                argument = self.fetch()

            return opname, argument

        # ...

It takes some bytecode and constants, and keeps a running index of the current
bytecode position. It is wise to split the translation up into fetch and decode
steps. The `fetch` method simply retrieves the next bytecode, while `decode`
will fetch the opcode, look up its name and fetch any arguments.

We need to look up which registers holds which variable:

    def variable(self, number):
        # AMD64 argument passing order for our purposes.
        order = ("rdi", "rsi", "rdx", "rcx")
        return order[number]

The main method will look like


    def compile(self):
        while self.index < len(self.bytecode):
            op, arg = self.decode()

            if op == "LOAD_FAST":
                yield "push", self.variable(arg), None
            # ...
            else:
                raise NotImplementedError(op)

Here you can already see how we translate `LOAD_FAST`. We just push the
corresponding register onto the stack. So, if the function we are compiling has
one argument, the bytecode will refer to the zeroth variable. Through the
`variable` method, we see that this is register RDI. So it will output

    ("push", "rdi", "None")

The `STORE_FAST` instruction does the reverse. It pops a value off the stack
and stores it in the argument register:

        yield "pop", "rax", None
        yield "mov", self.variable(arg), "rax"

A binary instruction will pop two values off the stack. For example

        elif op == "BINARY_MULTIPLY":
            yield "pop", "rax", None
            yield "pop", "rbx", None
            yield "imul", "rax", "rbx"
            yield "push", "rax", None

That's just about it. `LOAD_CONST` will use a special instruction for storing
immediate values (i.e., constant integers). Here is the entire method:

    def compile(self):
        while self.index < len(self.bytecode):
            op, arg = self.decode()

            if op == "LOAD_FAST":
                yield "push", self.variable(arg), None

            elif op == "STORE_FAST":
                yield "pop", "rax", None
                yield "mov", self.variable(arg), "rax"

            elif op == "LOAD_CONST":
                yield "immediate", "rax", self.constants[arg]
                yield "push", "rax", None

            elif op == "BINARY_MULTIPLY":
                yield "pop", "rax", None
                yield "pop", "rbx", None
                yield "imul", "rax", "rbx"
                yield "push", "rax", None

            elif op in ("BINARY_ADD", "INPLACE_ADD"):
                yield "pop", "rax", None
                yield "pop", "rbx", None
                yield "add", "rax", "rbx"
                yield "push", "rax", None

            elif op in ("BINARY_SUBTRACT", "INPLACE_SUBTRACT"):
                yield "pop", "rbx", None
                yield "pop", "rax", None
                yield "sub", "rax", "rbx"
                yield "push", "rax", None

            elif op == "UNARY_NEGATIVE":
                yield "pop", "rax", None
                yield "neg", "rax", None
                yield "push", "rax", None

            elif op == "RETURN_VALUE":
                yield "pop", "rax", None
                yield "ret", None, None
            else:
                raise NotImplementedError(op)

We can now compile the `foo` function at the top to our IR.

    >>> def foo(a, b):
    ...   return a*a - b*b
    ...
    >>> bytecode = map(ord, foo.func_code.co_code)
    >>> constants = foo.func_code.co_consts
    >>> ir = Compiler(bytecode, constants).compile()
    >>> ir = list(ir)
    >>>
    >>> from pprint import pprint
    >>> pprint(ir)
    [('push', 'rdi', None),
     ('push', 'rdi', None),
     ('pop', 'rax', None),
     ('pop', 'rbx', None),
     ('imul', 'rax', 'rbx'),
     ('push', 'rax', None),
     ('push', 'rsi', None),
     ('push', 'rsi', None),
     ('pop', 'rax', None),
     ('pop', 'rbx', None),
     ('imul', 'rax', 'rbx'),
     ('push', 'rax', None),
     ('pop', 'rbx', None),
     ('pop', 'rax', None),
     ('sub', 'rax', 'rbx'),
     ('push', 'rax', None),
     ('pop', 'rax', None),
     ('ret', None, None)]

That's a lot of stack operations! We'd better write a simple optimizer.

Part three: Writing a simple optimizer
--------------------------------------

With the above code, we can see how the optimizer works:

    >>> pprint(optimize(ir))
    [('push', 'rdi', None),
     ('mov', 'rax', 'rdi'),
     ('pop', 'rbx', None),
     ('imul', 'rax', 'rbx'),
     ('push', 'rax', None),
     ('push', 'rsi', None),
     ('mov', 'rax', 'rsi'),
     ('pop', 'rbx', None),
     ('imul', 'rax', 'rbx'),
     ('mov', 'rbx', 'rax'),
     ('pop', 'rax', None),
     ('sub', 'rax', 'rbx'),
     ('mov', 'rax', 'rax'),
     ('ret', None, None)]

However, after one pass we have a few more situations that can be optimized.
The first three functions, for example, should be possible to optimize into

    mov rax, rdi
    mov rbx, rdi

Indeed, running it twice, it produces

    >>> pprint(optimize(optimize(ir)))
    [('mov', 'rbx', 'rdi'),
     ('mov', 'rax', 'rdi'),
     ('imul', 'rax', 'rbx'),
     ('push', 'rax', None),
     ('mov', 'rbx', 'rsi'),
     ('mov', 'rax', 'rsi'),
     ('imul', 'rax', 'rbx'),
     ('mov', 'rbx', 'rax'),
     ('pop', 'rax', None),
     ('sub', 'rax', 'rbx'),
     ('ret', None, None)]

After two passes, it won't be able to optimize further. An obvious way to fix
that would be to expand the `push x; other; pop y` optimization to handle an
arbitrary number of `other` instructions. We are also affected by our poor use
of registers, since we only use RAX and RBX. A good register allocated is very
important for good optimizations.

We are now ready for the final part.

The final part: Translating IR to x86-64 machine code
-----------------------------------------------------

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
