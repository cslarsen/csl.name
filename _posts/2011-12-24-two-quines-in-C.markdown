---
layout: post
title:  "Two quines in C"
subtitle: ""
date:   2011-12-24 12:00:00 +01:00
updated: 2013-12-01 23:41:00 +01:00
categories: C
---

This document is the HTML&dash;version of [a gist][gist] I made earlier.

A quine is a program that, when run, produces an exact copy of its original
source code.

In [Reflections on trusting trust][trust], [Ken Thompson][ken] urges the
reader to try write a quine.  If you haven't done so yet, please try before
reading on.

Here are two quines I made wrote in C&mdash;without cheating! They are both
based on the same idea. The first one is macro&dash;based, utilizing the
ability for macros to quote parameters.

{% highlight C %}
#define Q(x) #x;x
char *q=Q(main(){printf("#define Q(x) #x;x\nchar *q=Q(");printf(q);printf(")\n");})
{% endhighlight %}

Compiling and running it produces an exact copy of itself:

{% highlight bash %}
$ gcc --no-warnings shortq.c -o shortq && ./shortq
#define Q(x) #x;x
char *q=Q(main(){printf("#define Q(x) #x;x\nchar *q=Q(");printf(q);printf(")\n");})
{% endhighlight %}

In the next quine I tried to do away with macros.  First I tried quoting
program code, but then I had to quote quotation characters, and for each
compilation of the next quine, the quotation degraded quickly.  The trick I
used was just to use `putchar` instead.  Not extremely elegant, but simple
to understand.

{% highlight C %}
char*p="main(){putchar(99);putchar(104);putchar(97);putchar(114);putchar(42);putchar(112);putchar(61);putchar(34);printf(p);putchar(34);putchar(59);putchar(10);put
s(p);}";
main(){putchar(99);putchar(104);putchar(97);putchar(114);putchar(42);putchar(112);putchar(61);putchar(34);printf(p);putchar(34);putchar(59);putchar(10);puts(p);}
{% endhighlight %}

Compiling and running the above program produces

{% highlight bash %}
$ gcc --no-warnings putchar-quine.c -o p && ./p
char*p="main(){putchar(99);putchar(104);putchar(97);putchar(114);putchar(42);putchar(112);putchar(61);putchar(34);printf(p);putchar(34);putchar(59);putchar(10);puts(p);}";
main(){putchar(99);putchar(104);putchar(97);putchar(114);putchar(42);putchar(112);putchar(61);putchar(34);printf(p);putchar(34);putchar(59);putchar(10);puts(p);}
{% endhighlight %}

Now, let's ponder the last example for a bit. You can see that I'm just
writing out a bunch of numerical codes, with some boilerplate code to get it
all started. In other words, we have some <i>driver code</i> and then a
<i>payload</i> of specific data we print out. We can actually create a quine
out of <i>any</i> program. That is the main point in Thompson's article.

<div id="disqus_thread"></div><script type="text/javascript"
src="http://disqus.com/forums/christianstigenlarsen/embed.js"></script><noscript><p><a
href="http://christianstigenlarsen.disqus.com/?url=ref">View the discussion
thread.</a></p></noscript><a href="http://disqus.com"
class="dsq-brlink">blog comments powered by <span
class="logo-disqus">Disqus</span></a>

gist: https://gist.github.com/1517172
trust: http://cm.bell-labs.com/who/ken/trust.html
ken: http://en.wikipedia.org/wiki/Ken_Thompson
