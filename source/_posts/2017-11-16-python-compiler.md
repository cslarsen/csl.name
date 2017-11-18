---
layout: post
title: "JIT compiling a subset of Python to x86-64"
date: 2017-11-16 20:52:12 +0100
updated: 2017-11-16 20:52:12 +0100
categories: Python assembly
disqus: true
tags: Python assembly
---

This post shows how to write a basic JIT compiler for the Python bytecode,
using nothing but stock Python modules.

We will leverage the code written in [a previous post][previous-post] to bind
native code to callable Python functions. The complete code is available at
[github.com/cslarsen/minijit][minijit.github].

At the end of this post, we will be able to compile Python functions:

    >>> def foo(a, b):
    ...   return a*a - b*b
    ...
    >>> bar = compile_native(foo)
    >>> bar(1, 2)
    -3

The `bar` function above will execute on bare metal. We will restrict ourselves
to compiling branchless functions that operate on integer arithmetic.

Our strategy is to translate Python bytecode to an [intermediate
representation][ir.wiki], which will then be optimized before being emitted as
x86-64 machine code. So the first part will be to understand how the bytecode
works.

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
one will be used for the explanation. **I've picked Python 2.7 for the
remainder of this post**, but the [GitHub code][minijit.github] supports both
2.7 and 3+.

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
using the [shunting-yard algorithm][shunting-yard.wiki]:

    2 2 * 3 3 * -

Moving from left to right, we push 2 on the stack, then another 2. The `*` pops
them both off the stack and pushes the their product 4. Push 3 and 3, pop them
off and push their product 9. The stack will now contain 9 on the top and 4 at
the bottom. For the final subtraction, we pop them off, perform the subtraction
and push the result -5 on the stack.

Now, imagine that the expression was actually written in a programming
language.

    subtract(multiply(2, 2), multiply(3, 3))

The thing is, in postfix form, the order of evaluation becomes explicit:

    push 2, push 2, multiply, push 3, push 3, multiply, subtract

The use of a stack makes it possible to execute instructions linearly, and this
is essentially how stack machines operate. With that, you will probably
understand most of the [CPython opcodes][python.opcodes] and its [interpreter
loop][python.eval].

Part two: Translating Python bytecode to IR
-------------------------------------------

We will now translate the bytecode instructions to an [intermediate
representation (IR)][ir.wiki]. That is, in a form suitable for performing
things like analysis, translation and optimization. Ours will be blissfully
naive. We will forego things [single-static assignment (SSA)][ssa.wiki] and
[register allocation][register-allocation.wiki] for the sake of simplicity, we
will use something that resembles [three-address codes (TAC)][tac.wiki].

Our IR will consist of pseudo-assembly instructions in a list. For example

    ir = [("mov", "rax", 101),
          ("push", "rax", None)]

Contrary to TAC, we put operation first, followed by the destination and source
registers.  We use `None` to indicate unused registers and arguments.  It would
be a very good idea to use unique, abstract registers names like `reg1`, `reg2`
and so on, because it facilitates [register
allocation][register-allocation.wiki].  Out of scope.

We will reserve registers RAX and RBX for menial work like arithmetic, pushing
and popping.  RAX must also hold the return value, because that's the
convention.  The CPU already has a stack, so we'll use that as our data stack
mechanism.

Registers RDI, RSI, RDC and RCX will be reserved for variables and arguments.
Per [AMD64 convention][amd64.abi], we expect to see function arguments passed
in those registers, in that order. In real programs, the matter is a bit more
involved.

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

Wow, that sure is a lot of stack operations!

Part three: Writing a simple optimizer
--------------------------------------

We're going to perform [peephole optimizations][peephole.wiki] on our IR. Such
optimizations work on only a few instructions at at time, and translate them
equivalent but _better_ code. We will go for fewer instructions.

In the IR above, we see an obvious improvement. Instructions like

    push rdi
    pop rax

can surely be translated to

    mov rax, rdi

Let's write a function for that. We'll also eliminate nonsensical instructions
like `mov rax, rax`.

    def optimize(ir):
        def fetch(n):
            if n < len(ir):
                return ir[n]
            else:
                return None, None, None

        index = 0
        while index < len(ir):
            op1, a1, b1 = fetch(index)
            op2, a2, b2 = fetch(index + 1)
            op3, a3, b3 = fetch(index + 2)
            op4, a4, b4 = fetch(index + 3)

            # Removed no-op movs
            if op1 == "mov" and a1 == b1:
                index += 1
                continue

            # Short-circuit push x/pop y
            if op1 == "push" and op2 == "pop":
                index += 2
                yield "mov", a2, a1
                continue

            index += 1
            yield op1, a1, b1

Instead of showing that this actually works, we'll just throw in a few other
optimizations. Just note that writing such optimizations are deceptively
simple. It's very easy to do something that seem to make sense, only to see
your program crash.

A construct like

    mov rsi, rax
    mov rbx, rsi

can surely be translated to

    mov rbx, rax

so we'll add that as well:

    if op1 == op2 == "mov" and a1 == b2:
        index += 2
        yield "mov", a2, b1
        continue

Finally, the short-circuit of pop and push can be extended so that it works
over one or several unrelated instructions. Take

    push rax
    mov rsi, rax
    pop rbx

Since RAX isn't modified in `mov rsi, rax`, we can just write

    mov rsi, rax
    mov rbx, rax

We have to be careful that the middle instruction isn't a push, though.
So we'll add

    if op1 == "push" and op3 == "pop" and op2 not in ("push", "pop"):
        if a2 != a3:
            index += 3
            yield "mov", a3, a1
            yield op2, a2, b2
            continue

There is nothing wrong with supporting an indefinite amount of middle
instructions, but we won't do that here.

With these instructions, let's try to optimize the above IR. The complete
optimization function is

    def optimize(ir):
        def fetch(n):
            if n < len(ir):
                return ir[n]
            else:
                return None, None, None

        index = 0
        while index < len(ir):
            op1, a1, b1 = fetch(index)
            op2, a2, b2 = fetch(index + 1)
            op3, a3, b3 = fetch(index + 2)

            if op1 == "mov" and a1 == b1:
                index += 1
                continue

            if op1 == op2 == "mov" and a1 == b2:
                index += 2
                yield "mov", a2, b1
                continue

            if op1 == "push" and op2 == "pop":
                index += 2
                yield "mov", a2, a1
                continue

            if op1 == "push" and op3 == "pop" and op2 not in ("push", "pop"):
                if a2 != a3:
                    index += 3
                    yield "mov", a3, a1
                    yield op2, a2, b2
                    continue

            index += 1
            yield op1, a1, b1

The IR code was

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

Running that through `optimize` yields

    >>> pprint(list(optimize(ir)))
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

saving us four instructions. But we still got a few spots left. The first three
instructions should be optimizable. Let's run two passes on the IR:

    >>> pprint(list(optimize(list(optimize(ir)))))
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

We've now saved seven instructions. Our optimizer won't be able to improve this
code any further. We could add more peephole optimizations, but another good
technique would be to use a real register allocated so that we use the full
spectrum of available registers. The IR compiler could then just assign values
to unique registers like `reg1`, `reg2` and so on, then the allocator would
choose how to populate the available registers properly. This is actually a hot
topic for research, and especially for JIT compilation because the general
problem is NP-complete.

Part four: Translating IR to x86-64 machine code
------------------------------------------------

So, we have translated Python bytecode to IR and we have done some
optimizations on it. We are finally ready to assemble it to machine code!

Our approach will be to write an assembler class that emits instructions. If we
use the same name for the emitter methods as in the IR code, and use the same
signature for all, then we can just blindly assemble the whole IR in a short
loop:

    assembler = Assembler(mj.PAGESIZE)

    for name, a, b in ir:
        emit = getattr(assembler, name)
        emit(a, b)

If the instruction is `mov rax, rbx`, then `emit` will point to `assembler.mov`
and the call will therefore be `assembler.mov("rax", "rbx")`.

Let's write an assembler class. We copy the code for `address`, `little_endian`
and import `create_block` from the [code in the previous post][previous-post].

    class Assembler(object):
        def __init__(self, size):
            self.block = mj.create_block(size)
            self.index = 0
            self.size = size

        @property
        def address(self):
            """Returns address of block in memory."""
            return ctypes.cast(self.block, ctypes.c_void_p).value

        def little_endian(self, n):
            """Converts 64-bit number to little-endian format."""
            return [(n & (0xff << i*2)) >> i*8 for i in range(8)]

        def emit(self, *args):
            """Writes machine code to memory block."""
            for code in args:
                self.block[self.index] = code
                self.index += 1

        def ret(self, a, b):
            self.emit(0xc3)

        # ...

So calling `assembler.ret(None, None)` will set the first machine code byte to
0xc3. That's how `retq` is encoded. To find the encoding of other instructions,
I mainly used the [NASM][nasm] assembler. Putting the following in a file
`sandbox.asm`,

    bits 64
    section .text
    mov rax, rcx
    mov rax, rdx
    mov rax, rbx
    mov rax, rsp

I assembled it with

    $ nasm -felf64 sandbox.asm -osandbox.o

(`-fmacho64` for macOS) and dumped the machine code with

    $ objdump -d sandbox.o

    sandbox.o:     file format elf64-x86-64


    Disassembly of section .text:

    0000000000000000 <.text>:
       0:   48 89 c8                mov    %rcx,%rax
       3:   48 89 d0                mov    %rdx,%rax
       6:   48 89 d8                mov    %rbx,%rax
       9:   48 89 e0                mov    %rsp,%rax

It seems like the 64-bit `movq` (which _we_ just call `mov`) is encoded with
the prefix `0x48 0x89` with the source and destination registers stored in the
last byte.  Digging into a few manuals, we see that they are encoded using
three bits each.  We'll write a method for that.

    def registers(self, a, b=None):
        """Encodes one or two registers for machine code instructions."""
        order = ("rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi")
        enc = order.index(a)
        if b is not None:
            enc = enc << 3 | order.index(b)
        return enc

For the `movq` instruction, we can now write

    def mov(self, a, b):
        self.emit(0x48, 0x89, 0xc0 | self.registers(b, a))

The rest of the instructions are done in a similar manner, except for moving
immediate (i.e., constant) values into registers.

    def ret(self, a, b):
        self.emit(0xc3)

    def push(self, a, _):
        self.emit(0x50 | self.registers(a))

    def pop(self, a, _):
        self.emit(0x58 | self.registers(a))

    def imul(self, a, b):
        self.emit(0x48, 0x0f, 0xaf, 0xc0 | self.registers(a, b))

    def add(self, a, b):
        self.emit(0x48, 0x01, 0xc0 | self.registers(b, a))

    def sub(self, a, b):
        self.emit(0x48, 0x29, 0xc0 | self.registers(b, a))

    def neg(self, a, _):
        self.emit(0x48, 0xf7, 0xd8 | self.register(a))

    def mov(self, a, b):
        self.emit(0x48, 0x89, 0xc0 | self.registers(b, a))

    def immediate(self, a, number):
        self.emit(0x48, 0xb8 | self.registers(a), *self.little_endian(number))

The only special thing about the last method is that we have to use a different
prefix and encode the number in little-endian format.

The final part
--------------

Finally, we can tie everything together. Given the function

    def foo(a, b):
      return a*a - b*b

we first extract the Python bytecode, using `ord` to map bytes to integers, and
any constants

    bytecode = map(ord, foo.func_code.co_code)
    constants = foo.func_code.co_consts

Compiling to IR

    ir = Compiler(bytecode, constants).compile()
    ir = list(ir)

Perform a few optimization passes:

    ir = list(optimize(ir))
    ir = list(optimize(ir))
    ir = list(optimize(ir))

Assemble to native code

    assembler = Assembler(mj.PAGESIZE)
    for name, a, b in ir:
        emit = getattr(assembler, name)
        emit(a, b)

Make the memory block executable

    mj.make_executable(assembler.block, assembler.size)

We use `ctypes` to set the correct signature and cast the code to a callable
Python function. We can get the number of arguments with `co_argcount`, and we
treat input arguments as signed 64-bit integers.

    argcount = foo.func_code.co_argcount
    signature = ctypes.CFUNCTYPE(*[ctypes.c_int64] * argcount)
    signature.restype = ctypes.c_int64

Finally,

    native_foo = signature(assembler.address)
    print(native_foo(2, 3))

It prints -5. Yay!

To disassemble the code, we can use the [Capstone disassembler][capstone] right
within Python. It's not a built-in module, so you need to install it yourself.
Or you can break into the Python process with a debugger. Here is the
disassembly for `native_foo`:

    0x7f1133351000:       mov     rbx, rdi
    0x7f1133351003:       mov     rax, rdi
    0x7f1133351006:       imul    rax, rbx
    0x7f113335100a:       push    rax
    0x7f113335100b:       mov     rbx, rsi
    0x7f113335100e:       mov     rax, rsi
    0x7f1133351011:       imul    rax, rbx
    0x7f1133351015:       mov     rbx, rax
    0x7f1133351018:       pop     rax
    0x7f1133351019:       sub     rax, rbx
    0x7f113335101c:       ret

You can try out different functions, for example

    def bar(n):
      return n * 0x101

turns into

    0x7f07d16a7000:       mov     rbx, rdi
    0x7f07d16a7003:       movabs  rax, 0x101
    0x7f07d16a700d:       imul    rax, rbx
    0x7f07d16a7011:       ret

and

    def baz(a, b, c):
      a -= 1
      return a + 2*b -c

becomes

    0x7f13fba09000:       push    rdi
    0x7f13fba09001:       movabs  rax, 1
    0x7f13fba0900b:       mov     rbx, rax
    0x7f13fba0900e:       pop     rax
    0x7f13fba0900f:       sub     rax, rbx
    0x7f13fba09012:       mov     rdi, rax
    0x7f13fba09015:       push    rdi
    0x7f13fba09016:       movabs  rax, 2
    0x7f13fba09020:       mov     rbx, rax
    0x7f13fba09023:       mov     rax, rsi
    0x7f13fba09026:       imul    rax, rbx
    0x7f13fba0902a:       pop     rbx
    0x7f13fba0902b:       add     rax, rbx
    0x7f13fba0902e:       mov     rbx, rdx
    0x7f13fba09031:       sub     rax, rbx
    0x7f13fba09034:       ret

You may wonder how fast this runs. The answer is: Slow. The reason is: Because
there is inherent overhead when calling into native code with `ctypes`. It
needs to convert arguments and so on. I also believe (but haven't
double-checked) that it saves some registers, because per the convention, we
should have restored RBX before exiting.

But it would be interesting to compile larger functions with native function
calls, loops and so on, and compare that with Python. At that point, I believe
you'll see the native code going much faster.

JITing automatically
--------------------

On [/r/compsci][reddit.comscpi] there was a comment that this really isn't
just-in-time compilation until there is some mechanism that automatically swaps
out a function with a compiled version. So let's try to do something about
that.

A pretty obvious approach is to require a little help from the user. Use a
decorator. Recall that a decorator is really just a function that gets the
freshly defined object as the first argument. If we install a little closure
there that remembers the original function, we can then *literally* compile
just-in-time when it is called for the first time. Again, *only* the decorated
functions that are actually called will be compiled to native code.

We'll start without anything:

    def jit(function):
        def frontend(*args, **kw):
            # Just pass on the call to the original function
            return function(*args, **kw)
        return frontend

With this, we can mark functions that we want to be compiled:

    @jit
    def foo(a, b):
        return a*a - b*b

So the inner `frontend` function then just needs to check if the function has
already been compiled. If not, compile it and install it as the local function
reference. If the compilation fails, don't swap out anything. The complete
decorator looks like this:

    def jit(function):
        def frontend(*args, **kw):
            if not hasattr(frontend, "function"):
                # We haven't tried to compile the function yet.
                try:
                    # Compile function and set it as the active one
                    native, asm = compile_native(function, verbose=False)
                    frontend.function = native
                except Exception as e:
                    # Oops, the compilation failed. Just fall silently back to
                    # the original function.
                    frontend.function = function

            # Call either the original or compiled function with the
            # user-supplied arguments
            return frontend.function(*args, **kw)

        # Make all calls to the decorated function go through "frontend"
        return frontend

See the GitHub repository for an example program that uses this.

What's next?
------------

I believe this is good for learning, so play around a bit. Try to make a
register allocator, for example. Create more peephole optimizations. Add
support for calling other functions, loops.

With a decorator, you should be able to swap out class methods on the fly with
compiled ones. That's exactly what  [Numba][numba] does, but ours is just a
drop in the ocean compared to that.

While we took the approach of translating Python bytecode, another good
technique is to use the `ast` module to traverse the abstract syntax tree. A
guy [did that][pyast64].

[amd64.abi]: https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf
[capstone]: http://www.capstone-engine.org/lang_python.html
[constant-folding]: https://en.wikipedia.org/wiki/Constant_folding
[cpython-eval]: https://github.com/python/cpython/blob/1896793/Python/ceval.c#L1055
[github]: https://github.com/cslarsen/minijit
[hn.front]: https://news.ycombinator.com/front?day=2017-11-09
[hn]: https://news.ycombinator.com/item?id=15665581
[ir.wiki]: https://en.wikipedia.org/wiki/Intermediate_representation
[minijit.github]: https://github.com/cslarsen/minijit
[mj.github]: https://github.com/cslarsen/minijit
[nasm]: http://www.nasm.us
[numba]: https://numba.pydata.org/
[peephole.wiki]: https://en.wikipedia.org/wiki/Peephole_optimization
[previous-post]: /post/python-jit/
[pyast64]: https://github.com/benhoyt/pyast64
[python.eval]: https://github.com/python/cpython/blob/1896793/Python/ceval.c#L1055
[python.opcodes]: https://github.com/python/cpython/blob/master/Include/opcode.h
[reddit.comscpi]: https://www.reddit.com/r/compsci/comments/7dfic7/jit_compiling_a_tiny_subset_of_python_to_x8664/
[register-allocation.wiki]: https://en.wikipedia.org/wiki/Register_allocation
[registers.wiki]: https://en.wikipedia.org/wiki/Processor_register
[rpn.wiki]: https://en.wikipedia.org/wiki/Reverse_Polish_notation
[shunting-yard.wiki]: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
[ssa.wiki]: https://en.wikipedia.org/wiki/Static_single_assignment_form
[stack-machine]: https://en.wikipedia.org/wiki/Stack_machine
[stack-register.wiki]: https://en.wikipedia.org/wiki/Stack_register
[tac.wiki]: https://en.wikipedia.org/wiki/Three-address_code
