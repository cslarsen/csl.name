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
