---
layout: post
title: "Making a simple VM interpreter in Python"
date: 2015-01-28 21:59:50 +0100
updated: 2015-01-28 21:59:50 +0100
categories: Python
disqus: true
tags: Python
---

<p class="lead">

I was asked if I could write a simple blog post on how to make a
<a href="https://en.wikipedia.org/wiki/Stack_machine">(process) virtual
machine</a> &mdash; specifically, a
<a href="https://en.wikipedia.org/wiki/Virtual_machine">stack machine</a>.
Using Python, I'm going to convince you how easy it is!

</p>

A stack machine doesn't have [registers][register-machine]. Instead, it puts
values on a stack and operate on that.  Stack machines are extremely simple,
but very powerful. There's a reason why Python, Java, PostScript, Forth and
many other languages have chosen a stack machine as their VM.

Anyway, let's talk a little bit more about the stacks. We need an instruction
pointer stack, which we'll use to store return addresses. This is so that we
can call a subroutine (a function, for instance)and then jump back from where
we came. We *could* have used self-modifying code instead, like Donald Knuth's
original [MIX][mix] did, but then you'd have to manage a stack yourself if you
wanted recursion to work.  In this post, I won't really implement subroutine
calls, but they're trivial anyway (consider it an exercise).

With the _data stack_ you get a lot of stuff for free.  For instance, consider
an expression like `(2+3)*4`.  On a stack machine, the equivalent code for this
expression would be `2 3 + 4 *`: Push two and three on the stack, then the next
instruction is `+`, so pop off two numbers and push back the sum.  Next, push
four on the stack, then pop two values and push their product. Easy!

So, let's start writing a simple class for a stack:

    class Stack:
        def __init__(self):
            self._values = []

        def pop(self):
            if len(self._values) == 0:
                raise RuntimeError("Stack underflow")
            else:
                return self._values.pop()

        def push(self, value):
            self._values.append(value)

        def top(self):
            return self._values[-1]

Nothing fancy, just push, pop and top operations.

Next, let's make a class for the machine itself. As noted, we need two stacks
and also some memory for the code itself. We'll rely on Python's dynamic typing
so we can put anything in the list. Only problem is that we really don't
discern between strings and built-in functions. The correct way would be to
insert actual Python functions in the list. Perhaps I'll do that in a future
update.

We also need an instruction pointer, pointing to the next item to execute in
the code.

    class Machine:
        def __init__(self, code):
            self.data_stack = Stack()
            self.return_addr_stack = Stack()
            self.instruction_pointer = 0
            self.code = code

Now we'll create some convenience functions to reduce some typing:

        def pop(self):
            return self.data_stack.pop()

        def push(self, value):
            self.data_stack.push(value)

        def top(self):
            return self.data_stack.top()

We'll create a `dispatch` function that takes an "opcode" (we don't really use
codes, just dynamic types, but you get my point) and executes it. But first,
let's just create the interpreter loop:

        def run(self):
            while self.instruction_pointer < len(self.code):
                try:
                    opcode = self.code[self.instruction_pointer]
                    self.instruction_pointer += 1
                    self.dispatch(opcode)
                except KeyboardInterrupt:
                    return
                except EOFError:
                    return

As you can see, it does one thing, and does it pretty well: Fetch the next
instruction, increment the instruction pointer and then dispatch based on the
opcode.  The `dispatch` function is a tad more longer:

        def dispatch(self, op):
            dispatch_map = {
                "%":        self.mod,
                "*":        self.mul,
                "+":        self.plus,
                "-":        self.minus,
                "/":        self.div,
                "==":       self.eq,
                "cast_int": self.cast_int,
                "cast_str": self.cast_str,
                "drop":     self.drop,
                "dup":      self.dup,
                "if":       self.if_stmt,
                "jmp":      self.jmp,
                "over":     self.over,
                "print":    self.print_,
                "println":  self.println,
                "read":     self.read,
                "stack":    self.dump_stack,
                "swap":     self.swap,
            }

            if op in dispatch_map:
                dispatch_map[op]()
            else:
                if isinstance(op, int):
                    self.push(op) # push numbers on stack
                elif isinstance(op, str) and op[0]==op[-1]=='"':
                    self.push(op[1:-1]) # push quoted strings on stack
                else:
                  raise RuntimeError("Unknown opcode: '%s'" % op)

Basically, it looks up the opcode, and sees if it's a built-in function like
`*` or `drop` or `dup`.  By the way, those are [Forth][forth] words, a
brilliant language that you should check out.  In fact, the code you'll see
here is bascially a simple Forth.

Anyway, it looks up an opcode like `*`, sees it should call `self.mul` then
executes it. It looks like this:

        def mul(self):
            self.push(self.pop() * self.pop())

All the other functions are like this.  If it can't find the operation in the
dispatch map, it will first see if it's a number. Numbers are automatically
pushed on the data stack.  If it's a quoted string, it will push that.

So, there you have it! Congrats!

Let's define a few more operations, then write a program using our newly
designed virtual machine and [p-code language][p-code]:

        def plus(self):
            self.push(self.pop() + self.pop())

        def minus(self):
            last = self.pop()
            self.push(self.pop() - last)

        def mul(self):
            self.push(self.pop() * self.pop())

        def div(self):
            last = self.pop()
            self.push(self.pop() / last)

          def println(self):
              self.print_()
              sys.stdout.write("\n")
              sys.stdout.flush()

Let's write that `print((2+3)*4)` example in our code:

    Machine([2, 3, "+", 4, "*", "println"]).run()

You can try running it now, even!

Now, let's introduce a jump operation, a _go-to_ operation:

        def jmp(self):
            addr = self.pop()
            if isinstance(addr, int) and addr >= 0 and addr < len(self.code):
                self.instruction_pointer = addr
            else:
                raise RuntimError("JMP address must be a valid integer.")

It just changes the instruction pointer.  Now let's look at branching:

        def if_stmt(self):
            false_clause = self.pop()
            true_clause = self.pop()
            test = self.pop()

            # False values: False, 0, "", everyting else is true
            result = True
            if isinstance(test, bool) and test == False:
                result = False
            if isinstance(test, str) and len(test) == 0:
                result = False
            if isinstance(test, int) and test == 0:
                result = False

            if result == True:
                self.push(true_clause)
            else:
                self.push(false_clause)

This is also very straight-forward. If you wanted to add a conditional jump,
you'd have to simply do `test-value true-value false-value IF JMP`.

Here's a program that asks the user for two numbers, then prints their sum and
product:

    Machine([
        '"Enter a number: "', "print", "read", "cast_int",
        '"Enter another number: "', "print", "read", "cast_int",
        "over", "over",
        '"Their sum is: "', "print", "+", "println",
        '"Their product is: "', "print", "*", "println"
    ]).run()

The `over`, `read` and `cast_int` operations look like this:

        def cast_int(self):
            self.push(int(self.pop()))

        def over(self):
            b = self.pop()
            a = self.pop()
            self.push(a)
            self.push(b)
            self.push(a)

        def read(self):
            self.push(raw_input())

Here's a simple program that asks the user for a number, prints if it's even or
odd, then loops:

    Machine([
        '"Enter a number: "', "print", "read", "cast_int",
        '"The number "', "print", "dup", "print", '" is "', "print",
        2, "%", 0, "==", '"even."', '"odd."', "if", "println",
        0, "jmp" # loop forever!
    ]).run()

Now, some exercises for you: Create `call` and `return` commands. The `call`
will push its current address on the return stack, then call `self.jmp()`.
The `return` operation should simply pop the return stack and set the
instruction pointer to this value (jumps back, or _returns_ from a `call`).
When you've done that, you've got subroutines. 

The next step is of course to add a parser that can take a language and
_compile_ it to your VM.

[I've done all of this in C++][csl-stack-machine], so you can get some hints
from there.

Good luck!

Here is the complete code:

    #!/usr/Bin/env python

    import sys

    class Stack:
        def __init__(self):
            self._values = []

        def pop(self):
            if len(self._values) == 0:
                raise RuntimeError("Stack underflow")
            else:
                return self._values.pop()

        def push(self, value):
            self._values.append(value)

        def top(self):
            return self._values[-1]

    class Machine:
        def __init__(self, code):
            self.data_stack = Stack()
            self.return_addr_stack = Stack()
            self.instruction_pointer = 0
            self.code = code

        def pop(self): # convenience function
            return self.data_stack.pop()

        def push(self, value): # convenience function
            self.data_stack.push(value)

        def top(self): # convenience function
            return self.data_stack.top()

        def run(self):
            while self.instruction_pointer < len(self.code):
                try:
                    opcode = self.code[self.instruction_pointer]
                    self.instruction_pointer += 1
                    self.dispatch(opcode)
                except KeyboardInterrupt:
                    return
                except EOFError:
                    return

        def dispatch(self, op):
            dispatch_map = {
                "%":        self.mod,
                "*":        self.mul,
                "+":        self.plus,
                "-":        self.minus,
                "/":        self.div,
                "==":       self.eq,
                "cast_int": self.cast_int,
                "cast_str": self.cast_str,
                "drop":     self.drop,
                "dup":      self.dup,
                "if":       self.if_stmt,
                "jmp":      self.jmp,
                "over":     self.over,
                "print":    self.print_,
                "println":  self.println,
                "read":     self.read,
                "stack":    self.dump_stack,
                "swap":     self.swap,
            }

            if op in dispatch_map:
                dispatch_map[op]()
            else:
                if isinstance(op, int):
                    self.push(op) # push numbers on stack
                elif isinstance(op, str) and op[0]==op[-1]=='"':
                    self.push(op[1:-1]) # push quoted strings on stack
                else:
                    raise RuntimeError("Unknown opcode: '%s'" % op)

        # OPERATIONS FOLLOW:

        def plus(self):
            self.push(self.pop() + self.pop())

        def minus(self):
            last = self.pop()
            self.push(self.pop() - last)

        def mul(self):
            self.push(self.pop() * self.pop())

        def div(self):
            last = self.pop()
            self.push(self.pop() / last)

        def mod(self):
            last = self.pop()
            self.push(self.pop() % last)

        def dup(self):
            self.push(self.top())

        def over(self):
            b = self.pop()
            a = self.pop()
            self.push(a)
            self.push(b)
            self.push(a)

        def drop(self):
            self.pop()

        def swap(self):
            b = self.pop()
            a = self.pop()
            self.push(b)
            self.push(a)

        def print_(self):
            sys.stdout.write(str(self.pop()))
            sys.stdout.flush()

        def println(self):
            self.print_()
            sys.stdout.write("\n")
            sys.stdout.flush()

        def read(self):
            self.push(raw_input())

        def cast_int(self):
            self.push(int(self.pop()))

        def cast_str(self):
            self.push(str(self.pop()))

        def eq(self):
            self.push(self.pop() == self.pop())


        def if_stmt(self):
            false_clause = self.pop()
            true_clause = self.pop()
            test = self.pop()

            # False values: False, 0, "", everyting else is true
            result = True
            if isinstance(test, bool) and test == False:
                result = False
            if isinstance(test, str) and len(test) == 0:
                result = False
            if isinstance(test, int) and test == 0:
                result = False

            if result == True:
                self.push(true_clause)
            else:
                self.push(false_clause)

        def jmp(self):
            addr = self.pop()
            if isinstance(addr, int) and addr >= 0 and addr < len(self.code):
                self.instruction_pointer = addr
            else:
                raise RuntimError("JMP address must be a valid integer.")

        def dump_stack(self):
            print("Data stack (top first):")
            for v in reversed(self._values):
                print(" - type %s, value '%s'" % (type(v), v))


    if __name__ == "__main__":
        print("** Program 1: Runs the code for `print((2+3)*4)`")
        Machine([2, 3, "+", 4, "*", "println"]).run()

        print("\n** Program 2: Ask for numbers, computes sum and product.")
        Machine([
            '"Enter a number: "', "print", "read", "cast_int",
            '"Enter another number: "', "print", "read", "cast_int",
            "over", "over",
            '"Their sum is: "', "print", "+", "println",
            '"Their product is: "', "print", "*", "println"
        ]).run()

        print("\n** Program 3: Shows branching and looping (use CTRL+D to exit).")
        Machine([
            '"Enter a number: "', "print", "read", "cast_int",
            '"The number "', "print", "dup", "print", '" is "', "print",
            2, "%", 0, "==", '"even."', '"odd."', "if", "println",
            0, "jmp" # loop forever!
        ]).run()

[csl-stack-machine]: https://github.com/cslarsen/stack-machine
[p-code]: https://en.wikipedia.org/wiki/P-code_machine
[process-vm]: https://en.wikipedia.org/wiki/Virtual_machine#Process_virtual_machines
[register-machine]: https://en.wikipedia.org/wiki/Register_machine
[forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)
