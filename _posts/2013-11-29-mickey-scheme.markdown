---
layout: post
title:  "Mickey Scheme"
subtitle: "An R7RS interpreter"
date:   2013-11-30 11:24:00 +01:00
categories: scheme programming
---

This is just an example post on the [Mickey Scheme][mickey] programming language.

Here is an example:

{% highlight scheme %}
(define-syntax unless
  (syntax-rules ()
    ((_ test code)
     (if (not test) code))))
{% endhighlight %}

As you can see, that was a macro. Quite simple, nothing special bout it. It
uses the `syntax-rules` macro engine, which is default since R6RS and was
actually specified as an optinal addendum to R5RS. R6RS also has
`syntax-case`, which is superior in almost any way to `syntax-rules` but is
not widely adopted.

[mickey]: https://github.com/cslarsen/mickey-scheme
