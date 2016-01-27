---
layout: post
title: "The straight (call/cc) story"
date: 2016-01-27 17:35:18 +0100
updated: 2016-01-27 17:35:18 +0100
categories: programming
disqus: true
tags: scheme continuations
---

<p class="lead">
Once, I gave an <a href="https://speakerdeck.com/csl/r7rs-scheme">introductory
talk on R<sup>7</sup>RS Scheme</a> that included some neat examples using
macros and continuations. This article expands on that, intended for anyone
curious about Scheme.
</p>

If your programming language supports continuations, you can implement
[any control flow construct](controlflow) in native source code form. Meaning
you can import them without requiring any binary shared libraries:

    (import (goto-statements)
            (exception-handling)
            (restartable-exceptions)
            (coroutines)
            (cooperative-multithreading)
            (nondeterministic-programming))

I'll show some examples using plaing
href="http://trac.sacrideo.us/wg/wiki/R7RSHomePage">R<sup>7</sup>RS Scheme</a>.
That means I only get to use undelimited continuations (more on that below).

You can run the examples using [Chibi Scheme](Chibi Scheme), but to cater to a
wider audience, I've included some pseudo-JavaScript code as well.

Implementing goto
-----------------

There are many ways to explain what continuations are, but I will offer two
mental models that are very useful, with a minor trade-off with exactness.

A continuation represents the rest of the computation. A good way to think
about this is that whenever you capture a continuation, you essentially set a
label in the code that you can later jump back to. So it's a GOTO or JUMP
instruction, with the additional benefit of being able to pass along new values
that will replace the value at the location where it jumps to.

So, if you have a function that computes some mathematical expression, you can
surround one of the variables with such a label. Later on, you can back jump to this
label, but with a new value for that variable.

Here's an example in pseudo-code that, unintentionally, looks like JavaScript:

    label = null;

    function print_person(name, age) {
      print("%s is %d years old.", name, set_label(age));
    }

    // At this point, label is still null and cannot be jumped to. The first
    // time we call print_person, make_label will be called, giving label some
    // imaginary value.

    print_person("John Doe", 123); // prints "John Doe is 123 years old."

    // Now we can jump to the label and pass it the value of 500

    goto_label(500); // prints "John Doe is 500 years old."

The function `set_label` will insert the current code location into the
variable `label` and return its input argument `age` immediately. After that,
`goto_label` will jump back to the location pointed to by `label` and continue
execution there, but now with the value of 500.

Running the program should print

    John Doe is 123 years old.
    John Doe is 500 years old.

Notice that after jumping to the label location, the program will continue
running at the `set_label(age)` location. At this point in time, the value of
`500` will be returned to the `print` function, which proceeds to print its
arguments. Finally, `print_person` itself has to return, and the return
location this time around will be right after the `goto_label(500)` statement.

If we translate the example to Scheme, we can implement `set_label` and
`goto_label`.

    (import (scheme base)
            (scheme write))

    ; A handy function that prints all of its arguments
    (define (println . args)
      (for-each display args)
      (newline))

    ; ... insert set-label and goto-label here

    (define label #f)

    (define (print-person name age)
      (println name " is " (set-label age) " years old."))

    (print-person "John Doe" 123)
    (goto-label 500)

To implement the final two functions, we'll use `call/cc`. It has a special
form:

    (call/cc
      (lambda (continuation)
        <body>))

For now, just completely ignore how that works, and I'll explain it later. The
important point here is that `continuation` is actually a function that lets
you jump back to the `(call/cc ...)` block. This is called capturing the
current continuation. You can also pass it values. Just keep reading and you'll
understand.

Now, the definition of `set-label` uses the above form:

    (define (set-label initial-return-value)
      (call/cc
        (lambda (continuation)
          (set! label continuation)
          initial-return-value)))

The code above will (1) capture the current continuation and put it inside the
variable `continuation`, (2) store the continuation in the global variable
`label`and (3) return the value `initial-return-value`.

Remember that our goal is to be able to *jump back* to where the `age` variable
is used, but with new values.

Now, since `label` will eventually contain a continuation, the implementation
of `goto-label` is straight-forward:

    (define (goto-label new-value)
      (label new-value))

The final program can be run in [Chibi Scheme](chibi). Put the following in a
file called `print-person-0.scm`:

    (import (scheme base)
            (scheme write))

    (define (println . args)
      (for-each display args)
      (newline))

    (define (set-label initial-return-value)
      (call/cc
        (lambda (continuation)
          (set! label continuation)
          initial-return-value)))

    (define (goto-label new-value)
      (label new-value))

    (define label #f)

    (define (print-person name age)
      (println name " is " (set-label age) " years old."))

    (println "Value of label: " label)
    (print-person "John Doe" 123)

    (println "Value of label after calling print-person: " label)
    (goto-label 500)

    (println "Done!")

Executing it gives:

    $ chibi-scheme print-person-0.scm
    Value of label: #f
    John Doe is 123 years old.
    Value of label after calling print-person: #<procedure #f>
    John Doe is 500 years old.
    Done!

Now I'll suggest a few mental models that can be used to understand how
continuations work. After that, we'll revisit this example, but wrap everything
up in a neat little library, so that we can write

    (import (goto))

in our Scheme programs.

Continuations as call stack manipulations
-----------------------------------------

Knowing a little bit about implementation strategies for continuations is of
great help in understanding them.

Many Schemes use [continuation passing style](cps), but I think it's much
easier to think of it in terms of copying and reinstating the call stack.

Recall that a [call stack](callstack) is a collection of stack frames. Each
frame contains a return address, function arguments and perhaps also a
placeholder for a return value.

In the program above, imagine that the program is a tree. At the root node, the
program first has a branch out to the definition of println, whose internal
definitions branch out from there. Next, it has a branch for the definition for
`make-label`, and so on:

    root
    |
    +-- import -- ...
    |
    +-- define label -- ...
    |
    +-- define print-person -- ...
    |
    +-- print-person -- ...
    |
    +-- goto-with-value -- ...

    ^
    The top-level

The vertical line going from root and downwards --- the trunk, so to speak ---
is what we call the *top-level* in Scheme.

Now, what `call/cc` does, in some implementations, is to take a copy of the
entire call stack from the top-level up to the current position.

So we copy a chunk of the call stack and put it in memory. This is what we mean
when we say we "take a continuation".

When we want to invoke a continuation, we want to continue running at the same
location that the continuation was taken. To do this, we just reinstate the
call stack: Chop off the current call stack from the top-level up to the
current position, and replace it with the one we have stored. Finally, we have
a value to pass on (goto with *parameters*), so we place that in the correct
location at the very top of the stack where there is a place for the
reeturn-value. Then we continue running from there.

I'm doing some hand-waving here, of course. There are many other details that
you'd have to do, but the point is to understand the general strategy.

Creating a "goto" library using macros
--------------------------------------

Recall the goto example given earlier. It used a global variable `label` to
hold the continuation. This is a problem if we want to make a general
goto-library, because it means you can only ever jump to *one* location.

So we should take an extra argument to `set-label` and `goto-label`. But Scheme
passes arguments by value and cannot --- in general --- mutate their "outer"
values: A function taking an argument can only modify it locally.

There are two ways to solve this: With or without macros.

Without macros, we could use label *names* instead of *variable*, but that
requires a bit of house-keeping code. Let's use macros instead. We're going to
need them for more advanced examples anyway.

A *macro* will expand its body and embed it at the location in the code where
it's used. Think of it as a copy-and-paste operation that substitutes
variables. The general form is:

    (define-syntax <name>
      (syntax-rules ()
        ((<usage pattern 1> <expands to this>)
         (<usage pattern 2> <expands to this>
         ... and so on))))

The `syntax-rules` tells which macro system to use. Yes, there are several
available. The one we use is pattern-based. The next empty `()` is a list of
literals we *don't* want to substitute. In our case it's empty.

As a simple example, consider the `unless` function, which will execute some
code if its first boolean value is false:

    (define (unless test value)
      (if (not test) value))

Or in pseudo-code:

    function unless(test, value) {
      if ( !test )
        return value;
      else
        return null;
    }

The above definitions have a serious problem. Because they take *values*, which
means that they will always be evaluated. Imagine a `wipe-root` function that
deletes everything on your drive and returns the number of important files you
lost. What would happen below?

    (unless #t (wipe-root))

Well, it will actually execute `(wipe-root)` *before* passing on its return
value to `unless`. To work around that, we could wrap `wipe-root` in a closure:

    (unless #t (lambda () (wipe-root)))

or, equivalently

    unless(false, function() {
      wipe_root();
    });

But that doesn't look anything like the ordinary if-statement. Another solution
would be to take a function by value, `(unless #f wipe-root)`, but that
wouldn't be very useful.

Nay, the elegant solution is to use macros to *control evaluation*:

    (import (scheme base)
            (scheme write))

    (define-syntax unless
      (syntax-rules ()
        ((unless test code)
          (if (not test) code))))

    (define (wipe-root)
      (display "Wiping root ..."))

    (unless #t (wipe-root))

Running the above code in [Chibi](chibi) will not call `wipe-root`, and
therefore not print anything.

Note that some people dislike macros, because it can obscure the exact
behaviour of your program. For example, things that look like functions may
actually be macros, meaning you don't really know when --- or if --- your
arguments are evaluated, and that makes it hard to reason about your program.
I think they're great if used with care.

Anyway, back to libraries. To put `println` in a library, put this in a file
called `print.sld`. If you keep the `sld`-files in a separate directory, be
sure to pass the `-Ipath` option to Chibi.

    (define-library (print)
      (import (scheme base)
              (scheme write))
      (export println)
      (begin
        (define (println . args)
          (for-each display args)
          (newline))))

Now we can `(import (print))` in our code. Back to the goto library.  What we
want is to be able to write

    (import (scheme base)
            (scheme write)
            (goto))

    (let ((label #f))
      (make-label label)
      (write "Nyan ")
      (goto label))

and with value-passing:

    (import (scheme base)
            (scheme write)
            (print)
            (goto))

    (define *age* #f)
    (define *name* #f)

    (define (print-person name age)
      (println (set-label *name* name) " is "
               (set-label *age* age) " years old."))

    (print-person "John Doe" "123")
    (goto-label *name* "Jane Doe")
    (goto-label *age* "500")

We'll use the same strategy as before, except that `set-label` and `goto-label`
can be invoked using a label and value, or only a label.

    (define-library (goto)
      (import (scheme base)
              (scheme write)
              (scheme case-lambda))
      (export set-label
              goto-label)
      (begin
        (define goto-label
          (case-lambda
            ((label) (label '()))
            ((label value) (label value))))

        (define-syntax set-label
          (syntax-rules ()
            ((_ label value)
               (call/cc (lambda (k)
                          (if (not label) (set! label k))
                          value)))
            ((_ label)
               (set-label label '()))))))

The `goto-label` function uses `case-lambda`, which patterns matches on its
invocation form. The first line matches calls to `(goto-label <label>)`, while
the secod matches `(goto-label <label> <value>)`.

The `set-label` macro also matches on the same two patterns. Here we use a
single underscore instead of typing out the full name of the macro.

Put that in a file called `goto.sld`, and you should be able to run the above
examples.

Delimited and undelimited continuations
---------------------------------------

Using the call stack model of explanation explained earlier, it means that
taking a continuation copies the entire call stack down to the second-to-last
frame of the top-level. This prevents us from having endless loops whenever we
reinstate a continuation. It's a detail, don't worry.

Dealing with continuations in Scheme is done through the ``call/cc`` form. It
provides [undelimited continuations](undelimited), which --- while still
powerful --- are not as general as [delimited continuations](delimited).

Now, the big deal about continuations is that you can use them to implement
[any other control flow construct](controlflow), from simple gotos to
exception mechanisms, coroutines, cooperative threads, non-deterministic
programming and so on.

But it turns out that undelimited continuations cannot do this without storing
one additional piece of state. That is also somewhat of a detail, but as you'll
see in the later examples. we always have to keep tabs on different
continuations. The more general continuation systems are *delimited*. They are
truly functional, and do not require explicitly storing continuations in
variables. Many Schemes provide these through constructs such as
`shift`-`reset` or `prompt-abort`. I won't go into those, but the main idea is
that instead of copying the call stack, you can put a marker somewhere and only
copy a *part* of it. That means that your continuations will be *true*
functions.

Anyway, undelimited continuations are unfortunately not part of the official
R<sup>7</sup>RS specification, so I will focus on `call/cc` here.

A simple exception system
-------------------------

Anyway, let's dive right into the examples. Let's say we want to implement a
simple exception system. We can do that using ``call/cc`` and then users can
have exception handling with a simple import statement of pure Scheme code.

To make our exception library useful, we'll wrap the functionality in macros.

The basic idea is to have a global ``throw`` function that we pass the
continuation on to.

    (import (scheme base)
            (scheme write))

    (define throw (lambda () #f))

    (define-syntax try
      (syntax-rules (catch)
        ((try (catch thunk) body ...)
         (call/cc
           (lambda (exit)
             (set! throw
               (lambda (error)
                 (println "Exception: " error)
                 (exit)))
             body ...)))))

    (try
      (catch
        (lambda (error)
          (println "Error: " error)))

      (define (divide a b)
        (if (zero? b)
          (throw "Divide by zero")
          (println a "/" b " = " (/ a b))))

      (divide 10 2)
      (divide 1 3)
      (divide 3 0))

    (println "End of program")

Why would you care?
-------------------

Remember I used pseudo-JavaScript in the very first example? Well, many people
have talked about adding continuations to JavaScript. So it's better to learn
about it now than later.

What other cool stuff can you implement with continuations? Take a look at what
the Scala people are using them for. One cool usage that was made in Scheme was
a web server that could *serialize* continuations and send them across
processes. If I recall correctly, they used continuations to plug the stateless
hole you have when doing the server-client-server round dance, so that you
could program as if the user was there all the time, as in:

    name = ask_user("What is your name?");
    age = ask_user("What is your age?");
    print_to_user("Your name is %s and your age is %d", name, age);

The above program serves a complete HTML page to the user, asking his name.
If he chooses to answer --- and after any amount of time --- the program will
extract his reply, put it in the `name` variable and continue running as if
nothing had happened in between. In other words, we plug the statelessness hole
of HTTP using continuations, and can write programs that look like any other,
even though they go through an endless server-client round dance.

[callstack]: https://en.wikipedia.org/wiki/Call_stack#Structure
[chibi]: https://github.com/ashinn/chibi-scheme
[controlflows]: http://cstheory.stackexchange.com/q/16312/5442
[cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[delimited]: https://en.wikipedia.org/wiki/Delimited_continuation
[gls]: https://en.wikipedia.org/wiki/Guy_L._Steele,_Jr.
[undelimited]: https://en.wikipedia.org/wiki/Continuation
