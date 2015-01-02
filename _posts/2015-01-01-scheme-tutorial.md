---
layout: post
title:  "A Short R⁷RS Scheme Tutorial"
date:    2015-01-01 11:58:04 +01:00
updated: 2015-01-01 11:58:04 +01:00
categories: Scheme
disqus: true
tags: Scheme
---

<p class="lead">
This is a <em>work-in-progress</em> introduction to the Scheme programming
language. Specifically, it's aimed at the latest version, R<sup>7</sup>RS.
This little tutorial will be updated in the time ahead, so be sure to post
comments so I know what needs to be done!
</p>

About Scheme
------------

Scheme is a powerful and elegant language in the Lisp family of languages.
Many people find it to be elegant because its core is farily small. The [latest
specification][spec] runs only 80 pages and is very accessible. Use it as a
reference when learning Scheme.  In addition to being small, the entire
language itself is very close to pure lambda calculus: It is possible to base
the entire language on [*four* primitives][cowan-video]
([slides][cowan-slides]).  Because of this, the language is very cohesive and
fits logically together.

I want to be honest up front and mention some downsides as well. The language
is quite fragmented across implementations due to several reasons. First of
all, the specifications leave some details unspecified so as to keep it small
and leave enough wiggle room for implementations to decide how to do things.
Most importantly, there wasn't an official library system until R<sup>6</sup>RS
in 2007. Even worse, many people disagreed with its design, so the
new R<sup>7</sup>RS spec has its own system.  This means that code is not by
default portable between implementations.  In practice, this means that people
usually stick to one or two implementation silos. Andy Wingo gives some 
[good suggestions on how to pick one][wingo-impls].

Let's get started!
------------------

In this tutorial I will use the so-called example R<sup>7</sup>RS
implementation [Chibi Scheme][chibi-scheme].

When you invoke `chibi-scheme`, there are only two functions that are defined:
`import` and `cond-expand`. So to write "Hello, world!" you need to import
libraries.  `cond-expand` can be used to conditionally run code based on
particular features an implementation may have. It's like `#ifdef`s, if you
come from C.

Save the following in `hello-world.scm`.

<pre>
{% include scheme-tutorial/hello-world.scm %}
</pre>

This can be run by typing

    $ chibi-scheme hello-world.scm
    Hello, world!

Yeah, I almost forgot. You probably know that in Lisp we use _s-expressions_ to
write both code and data.  It means that everything is in prefix form, so that
an expression like `2 * (3 + 4)` must be written as `(* 2 (+ 3 4))`.
Personally I really love this way of writing programs, for several reasons:
It's terse but readable, it's machine-readable, there aren't syntactical ambiquities, and so on.
In particular, I like that *scope* is extremely visible because of the
parenthesis, and you work very close to the [AST][ast].

Variables
---------

You can define variables by using `define` and change existing ones using
`set!`.  Almost anything in Scheme is a first class citizen, so you use
`define` to bind functions to variables.  Here's a `cube` function:

    (define cube
      (lambda (n)
        (* n n n)))

What's going on here is that we create a function that takes one parameter `n`.
By default, the *last* expression is used as a return value, and here that will
be `(* n n)`.  We bind this function to the variable `cube`.  To cube a number,
you just call `(cube 12)`.

Since function definitions are so common, we can leave out the `lambda` by
using the shorthand form `(define (cube n) ...)`.  But sometimes, e.g. if you
want to return functions, it may be better to use a plain `lambda`.

Libraries
---------

The function `display` takes one or two arguments: An object to print and an
optional *port* --- an output destination like a file or a string buffer.

However, it only takes one argument, and is thus cumbersome to work with. E.g.,
to print a number and a string, we'd have to do

    (display (string-append "12^3 = " (number->string (cube 12))))

Let's create a small family of variadic `print` functions that all print to the
default output port. We'll bundle them up in a library.

<pre>
{% include scheme-tutorial/print.sld %}
</pre>

To use them, you need to `(import (print))` in your code.  The implementations
differ a bit in how they handle libraries.  E.g., Chibi Scheme requires that
`(define-library ...)` be in a separate file with the same name as the library
name. Also, with Chibi Scheme you can specify library search paths using the
`-I` option.

Proper Tail Recursion
---------------------

The Scheme specification requires that implementations are *properly tail
recursive*.  To explain this, let's make a short detour.

One thing that all programming languages have is an [abstract syntax tree][ast]
(AST). In Lisp dialects, this is very explicit. You're essentially coding very
close to the AST. This has several benefits, but I'll only mention one. It
becomes very clear how code is evaluated.

For instance, consider the following factorial function written in Java.

    public static int fact(int n)
    {
      if ( n == 0 )
        return 1;
      else
        return n*fact(n-1);
    }

The AST for this function could be something like

<p>
  <img src="/gfx/post/scheme-tutorial/ast-java-fact.svg"
       class="img-responsive center-block"
       style="max-height: 384px;"
       alt="Java factorial AST" />
</p>

Now, the way to evaluate this AST is to start at the top node, then descend to
each child, left-to-right.  If we do that, we can write out the scheme code
directly.  The only difference is that we'll use `equal?` instead of `==`.

    (if (equal? n 0) 1
      (* n (fact (- n 1))))

If you take a good look at this s-expression, you'll see that it is an *exact*
representation of the AST.

The reason I'm showing you this is because I want to talk about the tail call
elimination that all compliant Scheme implementations have.  It lets you write
recursive functions that will never blow up the stack: Every active tail call
is associated with a constant amount of stack space.  But this is *only*
possible when the *last thing* a function does is to *perform a function call*.

Traverse the AST depth first from left to right. What's the last thing the
function does before returning, assuming n is nonzero? Well, it has to multiply
`n` and the result of `(fact (- n 1))`.  It also means that this function may
blow up the stack for big numbers. But if we arrange so that the last thing it
does is to call itself, Scheme will use tail call optimization.

Looking at the AST again, we'll remove the nodes `*` and its child `n` and move
`fact` up so that it's a child of the `if` tree.  We'll then add an accumulator
that computes the result for us, or `(* n acc)`.

<p>
  <img src="/gfx/post/scheme-tutorial/ast-fact-tail.svg"
       class="wrapper img-responsive center-block"
       style="max-height: 324px;"
       alt="Tail-recursive factorial" />
</p>

Since we'll now take two parameters, we'll call the function `fact-helper`.
Also, instead of doing `(equal? n 0)` we'll just use `(zero? n)`.

    (define (fact-helper n acc)
      (if (zero? n) acc
        (fact-helper (- n 1)
                     (* n acc))))

For the final polish, we'll create a front-end function `fact`.

    (define (fact n)
      (define (fact-helper n acc)
        (if (zero? n) acc
          (fact-helper (- n 1)
                       (* n acc))))
      (fact-helper n 1))

Now, the function is tail recursive and thus will never blow up the stack. In
fact, a good implementation will reuse the stack frame used to call
`fact-helper` so that each call is simply a jump instruction; as fast as an
iterative version.

Macros
------

Macros is a way to rewrite code and control evaluation.  It's very important to
remember that macros are *always* and *only* expanded at *compilation time*.

{% callout warning %}
Scheme macros are *hygienic*.  It means that you'll when using identifiers
local to your macro, they will never collide with identifiers at run-time.
This is a good thing, but one downside is that you can't write <a
href="https://en.wikipedia.org/wiki/Anaphoric_macro">anaphoric macros</a>.
However, while R<sup>7</sup>RS only specifies hygienic macros using `syntax-rules`, most
implementations also provide for a `defmacro`-like system.
{% endcallout %}

Let's create a new branching macro.  It will be called `when`, and we want to
be able to say `(when true-or-false do-something)`.

Why can't this be a function? Because we don't want to evaluate arguments in
the case that the predicate is false.  For instance, if `when` was a function,
then

    (when #false (format-harddrive))

would call `(format-harddrive)`.  I'll just show you how you can do this using
`syntax-rules` right now, and in a later update I'll explain what's going on.

    (define-syntax when
      (syntax-rules ()
        ((when test code ...)
         (if test (begin code ...)))))

Continuations
-------------

The Scheme standard only has *undelimited* continuations via `call/cc`, but
many implementations offer delimited continuations as well.

This section will be covered later. Check back for updates!

[spec]: http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf
[cowan-video]: http://vimeo.com/29391029
[cowan-slides]: http://ccil.org/~cowan/scheme-2011-09.pdf
[wingo-impls]: http://wingolog.org/archives/2013/01/07/an-opinionated-guide-to-scheme-implementations
[chibi-scheme]: https://code.google.com/p/chibi-scheme/
[ast]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
[anaphoric]: https://en.wikipedia.org/wiki/Anaphoric_macro
