---
layout: post
title: "The straight call/cc story"
date: 2014-12-25 19:33:00 +01:00
updated: 2014-12-25 19:33:00 +01:00
categories: Scheme
disqus: true
tags: Lisp Scheme
---

<p class="lead">
I've read too many needlessly hard explanations for what <em>call/cc</em> is.
In essence, it's just what Guy Steele originally called <em>gotos with
parameters</em>.  If that doesn't make sense, think of them as being about
saving and restoring the entire call stacks.  Here I'll explain that and the
difference between delimited and undelimited continuations.
</p>

Gotos with parameters
--------------------------------

Forget what you've heard about continuations and time travelling.  When you see
`(call/cc ...)` in any part of your code, that is a location that you can
potentially jump back to.  If you want to, you can also pass a parameter that
will replace the entire `call/cc`-block with a value.  Evaluation then proceeds
as normal.

The _syntax_ for `call/cc` is almost the same as `lambda`, with the exception
that it only takes one parameter, and the return-value is what the entire
`call/cc` will evaluate to.

For instance,

    (+ 1 2 (call/cc (lambda (c) 3)))

is the same as

    (+ 1 2 3)


First-class call stacks
----------------------------------------------

The parameter `c` above is the current continuation.  That simply means that
`c` is a representation of the entire call-stack, right up to the top level.

The call stack (or, "continuation") can be saved to a variable, to be used
later. If you call the continuation, it will replace the current call stack
with it, so that effectively, you jump back to the location when it was made,
with the same values that were on the call-stack. You can also set the topmost
value.

For instance, let's make a small program that you can run through your
favourite Scheme interpreter.

    (define print-again #f)

    (define (print)
      (display (+ 1 2 (call/cc (lambda (c)
                                 (set! print-again c)
                                 3))))
      (newline))

    (print)
    (print-again 123)

Now, if you run this and call `(print)`, if will save the call stack into
`print-again` and evaluate to `(+ 1 2 3)` and print that.

But let's look at what the call stack will look like.  If we trace the program,
each time you evaluate a function, you create an entry on the call stack that
expects a return-value.  After creating a stack frame entry, if will
recursively create frames for each parameter. When these values have been
evaluated, it will then call into the main function again and set the
return-values in the stack frame. When a stack frame is entirely done, it will
be popped off the stack, and its return value will percolate back up again.

The call stack starts and ends at the top level.  Ok, so when you cann
`(print)` the first time, the stack will look like this:

    call print
      call display with arguments:
        call + with arguments: 1, 2 and <parameter>

As you can see, we have a call stack where we can insert any value at
`<parameter>`.  Since we've saved this continuation in `print-again`, when we
call `print-again 123`, the stack that will be reinstated is:

    call print
      call display with arguments:
        call + with arguments: 1, 2 and 123

The evaluator will continue from the top of the stack. It sees that `+` now has
all the values it needs, so it can now calculate what `(+ 1 2 123)` is. It will
then pop the top off the stack, setting the return value to `126`.  Next,
`display` is called, and the sole argument is simply th enumber `126`.  So it
will display that. The return value for `display` is unspecified, so there will
probably not be any values left on the stack after that. It will then conclude
`print` and continue from wherever the user was before.

So, as you can see, when we've saved the continuation, we can jump back to the
original location with new values as many times as we want.

Implementing a goto-mechanism
-----------------------------

We can actually implement a simple goto system in Scheme. Let's let the user
set positions with `(label name)` so you can do `(goto name value)` to jump
back to this location.

We can even do this without using macros.
