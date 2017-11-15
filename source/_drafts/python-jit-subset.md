---
layout: post
title: "More x86-64 goodness: JIT-compiling a subset of Python from scratch"
date: 2017-11-14 17:50:12 +0100
updated: 2017-11-14 17:50:12 +0100
categories: Python assembly
disqus: true
tags: Python assembly
---

This is a follow-up to the post <a href="/post/python-jit">Writing a basic
x86-64 JIT-compiler in stock Python</a> that made it to the <a
href="https://news.ycombinator.com/front?day=2017-11-09">front page of HN</a>.
In case you didn't read it: It shows how to execute arbitrary x86-64 machine
code at runtime in Python using the built-in `ctypes` module.

<b>In this post, we're going directly build upon that code to write a x86-64
JIT-compiler for a tiny subset of Python.</b> In other words, we'll be able to
do this:

    >>> def foo(a, b):
    ...   return a*a - b*b
    ...
    >>> bar = compile_native(foo)
    >>> bar(1, 2)
    -3

To keep the scope small, we'll restrict ourselves to branchless functions that
only perform integer arithmetic. We'll skip if-statements and loops for the
sake of simplicity.

The strategy is simply to translate Python bytecode into x86-64 machine code.

That way, Python will do most of the heavy-lifting for us. We don't need to
deal with parsing source code or abstract syntax trees. It will even throw in a
few optimizations for us, like [constant folding][constant-folding].  So, the
first part will be to understand how Python bytecode works.

Part one: How Python bytecode works
-----------------------------------

Skip this part if you know what a stack machine is.

The CPython interpreter is implemented as a stack machine, just like the JVM,
CLR, Forth and many other successful languages. A [stack
machine][stack-machine] consists of commands that operate on a data stack.

For example, multiplying two integers can be done with

<table align="center">
  <thead>
    <tr>
      <th>Instruction</th>
      <th>Argument</th>
      <th>Stack</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>LOAD_CONSTANT</code></td>
      <td><code>2</code></td>
      <td><code>[2]</code></td>
    </tr>
    <tr>
      <td><code>LOAD_CONSTANT</code></td>
      <td><code>3</code></td>
      <td><code>[3, 2]</code></td>
    </tr>
    <tr>
      <td><code>BINARY_MULTIPLY</code></td>
      <td><code></code></td>
      <td><code>[6]</code></td>
    </tr>
  </tbody>
</table>

The first instruction pushes 2 on the stack, the second 3, and the final one
pops the two off and push their product 6 on the stack.

One of the reasons why stack machines are nice is because you can easily
serialize computations involving subexpressions. For example, to calculate
`c*(a + b)`:

<table align="center">
  <thead>
    <tr>
      <th>Instruction</th>
      <th>Argument</th>
      <th>Stack</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>LOAD_FAST</code></td>
      <td><code>c</code></td>
      <td><code>[c]</code></td>
    </tr>
    <tr>
      <td><code>LOAD_FAST</code></td>
      <td><code>a</code></td>
      <td><code>[a, c]</code></td>
    </tr>
    <tr>
      <td><code>LOAD_FAST</code></td>
      <td><code>b</code></td>
      <td><code>[b, a, c]</code></td>
    </tr>
    <tr>
      <td><code>BINARY_ADD</code></td>
      <td><code></code></td>
      <td><code>[(a + b), c]</code></td>
    </tr>
    <tr>
      <td><code>BINARY_MULTIPLY</code></td>
      <td><code></code></td>
      <td><code>[c*(a + b)]</code></td>
    </tr>
  </tbody>
</table>

To see the disassembly of Python functions, you can use the `dis` module. For
example, the disassembly of the function at the top of the post is

    >>> import dis
    >>> dis.dis(foo)
      1           0 LOAD_FAST                0 (a)
                  3 LOAD_FAST                0 (a)
                  6 BINARY_MULTIPLY
                  7 LOAD_FAST                1 (b)
                 10 LOAD_FAST                1 (b)
                 13 BINARY_MULTIPLY
                 14 BINARY_ADD
                 15 RETURN_VALUE

In the above disassembly, the leftmost `1` is the source code line that is
currently being shown. Our function only had one line.  The next number is the
bytecode index. We can clearly see that `LOAD_FAST` is encoded using three
bytes. The first one encodes the opcode (i.e., which instruction it is) and the
last two encode the argument `0`. Its an index into an array of local
variables, which here will be `a`. Thus, the reference for `a` will be pushed
onto the stack.

The final `RETURN_VALUE` instruction simply returns to the caller with the
current top-of-stack value as the result of that function call. Exactly how it
does this is not very important, but it is common for callers to put the
desired return address on the stack before calling into a function (I don't
believe this is what CPython does, though). Many stack machines will also nuke
any remaining stack items before returning with the value. That's particularly
important when handling exceptions. If you're interested, you can look at
[CPython's main interpreter loop][cpython-eval].

Part II: The assembler
----------------------

How can we translate Python bytecode like

    LOAD_CONSTANT 1
    LOAD_CONSTANT 2
    BINARY_MULTIPLY
    RETURN_VALUE

to machine code? Well, [we've already got a stack][stack-register.wiki] to work
with, so we can use that to store items. For example, the first two
instructions can be encoded as

    mov rax, 1
    push rax

    mov rax, 2
    push rax

In case you're new to assembly, the `mov rax, 1` instruction will put the
number `1` in the [`rax` register][registers.wiki].  The multiplication
instruction could then be implemented as

    pop rax
    pop rbx
    imul rax, rbx
    push rax

while the `RETURN_VALUE` instruction could be just

    pop rax
    ret

And that's exactly how we're going to do things. First of all, we need a very
simple assembler. Its skeleton will be

    class Assembler(object):
        def __init__(self, size):
            self.block = mj.create_block(size)
            self.size = size
            self.index = 0

        def emit(self, *args):
            """Writes machine code to memory block."""
            for code in args:
                self.block[self.index] = code
                self.index += 1
        # ...

It will use `mj.py` [from the previous post][mj.github] as a module to create a
page-aligned block of memory. The variadic `emit` function lets us write bytes
into it. Next up, we need to know how to encode various x86-64 instructions
into machine code. That sounds deceptively simple. In fact, encoding
instructions is quite complicated. Fortunately, we won't need many instructions.
I used the [NASM][nasm] assembler along with `objdump` to figure out the
encoding of general instructions. I then used a few x86-64 manuals to figure
out how to encode a few arguments.

For example, encoding the `retq` instruction (return quad-word) is
straight-forward:

    def ret(self):
        self.emit(0xc3)

To find that with NASM, you can put the following in a file `sandbox.asm`:

    bits 64
    align 16
    section .text

    ret

Compile with `nasm -felf64 sandbox.asm -osandbox.o` on Linux and `-fmacho64` on
macOS. Then dump the code with `objdump -d sandbox.o`:

    sandbox.o:     file format mach-o-x86-64
    Disassembly of section .text:
    0000000000000000 <.text>:
       0:	c3                   	retq  

We can clearly see that `retq` consist simply of `0xc3`. An instruction like
`mov rax, 0x1122334455667788` is encoded as

    0: 48 b8 88 77 66 55 44  movabs $0x1122334455667788,%rax
    7: 33 22 11

From this we can deduce that we need to use `movabs` to enter immediate 64-bit
values into `rax`, that the instruction is encoded with the byte sequence `0x48
0xb8` with the remaining eight bytes contain the integer constant in
little-endian format.

The previous post showed how to encode a number in little-endian format. The
below one is a code-golfed version. I'm sure someone can make it even better.

    def little_endian(self, n):
        """Converts 64-bit number to little-endian format."""
        return [(n & (0xff << i*2)) >> i*8 for i in range(8)]

In the Assembler class, we'll emit the `movabs` instruction with

    def immediate(self, a, number):
        self.emit(0x48, 0xb8 | self.registers(a), *self.little_endian(number))

Here's when those x86-64 manuals come in handy. The target register is `OR`ed
into the second byte. Each register can be encoded as three bits, with `rax`
being `000`. The helper-method `registers` will encode up to two registers. Two
registers will be required for instructions like `mov rax, rbx`. Here it is.

    def registers(self, a, b=None):
        """Encodes one or two registers for machine code instructions."""
        order = ("rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi")
        enc = order.index(a)
        if b is not None:
            enc = enc << 3 | order.index(b)
        return enc

Thus, `rax` is `000`, `rcx` is `001` and so on. Speaking of which, here is the
general `mov` instruction that implements `mov register1, register2`:

      def mov(self, a, b):
          self.emit(0x48, 0x89, 0xc0 | self.registers(b, a))

With that, I'll just dump the entire Assembler class below. The other
instructions we will need are `push` and `pop` for working with the stack,
`imul` for signed integer multiplication, `add` and `sub` for addition and
subtraction and `neg` to negate a number in a register. There is also an
`address` property to get the address of the machine code block in memory.

    class Assembler(object):
        def __init__(self, size):
            self.block = mj.create_block(size)
            self.size = size
            self.index = 0

        @property
        def address(self):
            """Returns address of block in memory."""
            return ctypes.cast(self.block, ctypes.c_void_p).value

        def little_endian(self, n):
            """Converts 64-bit number to little-endian format."""
            return [(n & (0xff << i*2)) >> i*8 for i in range(8)]

        def registers(self, a, b=None):
            """Encodes one or two registers for machine code instructions."""
            order = ("rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi")
            enc = order.index(a)
            if b is not None:
                enc = enc << 3 | order.index(b)
            return enc

        def emit(self, *args):
            """Writes machine code to memory block."""
            for code in args:
                self.block[self.index] = code
                self.index += 1

        def ret(self):
            self.emit(0xc3)

        def push(self, a):
            self.emit(0x50 | self.registers(a))

        def pop(self, a):
            self.emit(0x58 | self.registers(a))

        def imul(self, a, b):
            self.emit(0x48, 0x0f, 0xaf, 0xc0 | self.registers(a, b))

        def add(self, a, b):
            self.emit(0x48, 0x01, 0xc0 | self.registers(b, a))

        def sub(self, a, b):
            self.emit(0x48, 0x29, 0xc0 | self.registers(b, a))

        def neg(self, a):
            self.emit(0x48, 0xf7, 0xd8 | self.register(a))

        def mov(self, a, b):
            self.emit(0x48, 0x89, 0xc0 | self.registers(b, a))

        def immediate(self, a, number):
            self.emit(0x48, 0xb8 | self.registers(a), *self.little_endian(number))

[constant-folding]: https://en.wikipedia.org/wiki/Constant_folding
[cpython-eval]: https://github.com/python/cpython/blob/1896793/Python/ceval.c#L1055
[mj.github]: https://github.com/cslarsen/minijit
[nasm]: http://www.nasm.us
[registers.wiki]: https://en.wikipedia.org/wiki/Processor_register
[stack-machine]: https://en.wikipedia.org/wiki/Stack_machine
[stack-register.wiki]: https://en.wikipedia.org/wiki/Stack_register
