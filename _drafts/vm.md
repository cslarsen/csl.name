---
layout: post
title: "Making a simple virtual machine interpreter"
date: 2015-01-28 21:59:50 +0100
updated: 2015-01-28 21:59:50 +0100
categories: Python
disqus: true
tags: Python
---

Making a [(process) virtual machine][vm] is quite easy. I'll show you how you can
make your own. I'll be using various languages.

First, let's make a [stack-based][stack-machine], [p-code interpreter][p-code]
in Python.  For operating on values, we need a _data stack_. To be able to
return from functions and support recursion, we'll use a _instruction stack_
for these. We'll store the program code in a block of memory, but it will be
read only. Note that deciding on your VM details, such as memory layout, if it
supports multithreading, concurrency, various levels of safety and so on is
actually the interesting and fun part of designing a VM.  But for now, let's
make it simple.

Alrighty! All interpreters use a loop that fetches the next instruction,
decodes and then executes it.  Basically, it will look like this:

    def interpreter_loop(instruction, data_stack, return_stack):
      


[csl-stack-machine]: https://github.com/cslarsen/stack-machine
[p-code]: https://en.wikipedia.org/wiki/P-code_machine
[process-vm]: https://en.wikipedia.org/wiki/Virtual_machine#Process_virtual_machines
[register-machine]: https://en.wikipedia.org/wiki/Register_machine
[stack-machine]: https://en.wikipedia.org/wiki/Stack_machine
[vm]: https://en.wikipedia.org/wiki/Virtual_machine
