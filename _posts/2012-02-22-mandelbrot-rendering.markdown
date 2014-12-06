---
layout: post
title:  "Rendering the Mandelbrot Set"
subtitle: "with an example implementation in javascript"
date:   2012-02-22
updated: 2014-12-06 13:52:00 +01:00
categories: Programming JavaScript
disqus: true
tags: JavaScript Mandelbrot Math
---

<p class="lead">
In 2012 I made a
<a href="https://en.wikipedia.org/wiki/Mandelbrot_set">Mandelbrot Set
renderer</a>. The result can be seen on
<a href="http://mandelbrot-set.com">mandelbrot-set.com</a>
and the code is available on 
<a href="https://github.com/cslarsen/mandelbrot-js">my GitHub page</a>.

Here I will explain how you can implement one yourself.
</p>

<img class="img-rounded img-responsive"
     style="width:640px; height:388px;"
     src="/gfx/post/mandelbrot.png"
     alt="The Mandelbrot Set" />

Theory
------

The famous [Mandelbrot set](https://en.wikipedia.org/wiki/Mandelbrot_set) is
a set of points in the complex plane.  In essence, what we want to find out
is if the iterative function C below will _converge_ to some constant or _diverge_
to infinity.

The definition is simply

>  z<sub>n+1</sub> = z<sub>n</sub><sup>2</sup> + c

with the initial condition simply formed by taking the coordinates in the
complex plane,

>  z<sub>0</sub> = c = x + iy

Pretend for a moment that you've never seen the Mandelbrot plot before.
By only looking at the definition above, can you guess how it would look like when
rendered? First we need to look at a few expansions of z expressed with 
<span>x</span> and <span>y</span>.

> z<sub>0</sub> = x + iy

> z<sub>1</sub> = (x + iy)<sup>2</sup> + (x + iy) =
>  (x<sup>2</sup> - y<sup>2</sup> + x) + i(2xy + y)

> z<sub>2</sub> = z<sub>1</sub><sup>2</sup> + (x + iy) = ...

So, as <span>n &rarr; &infin;</span>, for which values of <span>x</span> and
<span>y</span> will <span>z<sub>n</sub></span> converge, and for which will it
diverge?

Well, for large values of <span>|x|</span> and small for <span>|y|</span>
(points close to origo), the sequence might diverge, because
<span>x<sup>2</sup></span> would be dominating over <span>y<sup>2</sup></span>.
Diverged points are painted black, so we can guess that the plot will be black
in all directions some distance from origo.

<div class="bs-callout bs-callout-warning">
<h4>Divergence and convergence for complex numbers</h4>
<p>
The mathematical description here is very imprecise.
</p>

<p>
Starting with <span>x</span> and <span>y</span>, for each iteration you'll get
a new expression <span>a + ib</span>, i.e. a new point at <span>(a,b)</span>.
As you iterate, you jump to new points in the complex plane.  For
convergent sequences, we will jump closer and closer to an attractor point
&mdash; usually curving towards it instead of a straight line.
</p>

<p>
For the Mandelbrot set, it means that if
any point lands outside a circle with radius two around origo, the sequence
will diverge.
</p>

<p>
<a href="https://en.wikipedia.org/wiki/Complex_quadratic_polynomial#Critical_orbit">Read more on Wikipedia</a>.
</p>
</div>

But for points close to origo &mdash; say, <span>|x|</span> and
<span>|y|</span> less than 1 &mdash; we would expect it to converge, because
the product of two numbers less than one will always be smaller than each of
its parts, giving small values for <span>x<sup>2</sup> - y<sup>2</sup></span>.  So in a
circle around origo with a radius of one, we'd expect to see colored pixels.

But what if the signs of x and y differ? Then things quickly get more
complicated.  In fact, if we plot using a computer, we won't get a
nice colored disc centered at origo.  What we get is an infinitely complex and
fractured plot.

We can even zoom endlessly into the plot and it will <em>still</em> be as
non-uniform and complex as before.  While a disc would have a dimension of two,
a fractal has a so-called so-called [Hausdorff dimension](http://en.wikipedia.org/wiki/Hausdorff_dimension)
which is something in between a line and a plane.  We'd get a non-integer
dimension; a <em>fractal</em>.

Calculating the Mandelbrot Set
------------------------------

Calculating the Mandelbrot set numerically is easy.

Given the equations above, take any point <span>z<sub>0</sub> = (x, y)</span>
and then calculate <span>z<sub>1</sub> = (x+iy)<sup>2</sup> + (x+iy)</span> and
continue doing this.  For practical purposes, let's decide on a _threshold_
value.  If the magnitude of <span>z</span> &mdash; the distance to origo, or
<span>&sqrt;(x^2+y^2)</span>&mdash; ever becomes larger than this threshold
value we will assume that it will diverge into infinity.  If so, stop the
calculation and plot a _black dot_ at the current location.

<div class="bs-callout bs-callout-info">
For the Mandelbrot set, it can be shown that the threshold value is exactly two, i.e.
any sequence with a <span>|z<sub>n</sub>| > 2</span> will diverge.

<a href="http://www.mi.sanu.ac.rs/vismath/javier/b2.htm">Read more about this</a>.
</div>

If <span>|z|</span> has not exceeded the threshold value after a predecided
number of iterations (which we choose at will), we will assume that the current
parameters makes the function converge.  In this case, plot a non-black dot at
the current location.

Colorizing the plot
-------------------

I said above that if the function diverges, one should plot a non-black dot.
One could simply paint a white dot here.  But instead, maybe we want to get
an idea of _how fast_ the function is diverging to infinity at this point.

To do this, just take the current value of the number of steps performed
and _map_ that against a color spectrum, and paint that color.

So, functions diverging quickly will get about the same color.

Smooth coloring
---------------

If you use the number of iterations to pick a color, you'll get ugly color
bands in the plot.  There is a really cool trick to get smooth, gradual
color changes.

So, you <em>basically</em> calculate `Z = Z^2` until it diverges and make a note of
the iteration count.  What we really want, though, is a _fractional_
iteration count, so we can multiply that with a color value to get smooth
colors.

The trick is to note that when you calculate `Z = Z^2` you'll get values `Z,
Z^2, Z^4, Z^8` and so on.  If you take the logarithm of this, you'll get the
values 1, 2, 4, 8 etc.  If you take the logarithm one more time, you'll get
1, 2, 3, 4, 5 etc.  So to get a fractional number of iterations, just do:

    log(log |Z|) / log 2

This is all explained over at http://linas.org/art-gallery/escape/smooth.html

In my code, I originally used the following smoothing equation:

    1 + n - Math.log(Math.log(Math.sqrt(Zr*Zr+Zi*Zi)))/Math.log(2.0);

With some elementary logarithm rules, we can simplify this to

    // Some constants
    var logBase = 1.0 / Math.log(2.0);
    var logHalfBase = Math.log(0.5)*logBase;
    // ...
    return 5 + n - logHalfBase - Math.log(Math.log(Tr+Ti))*logBase;

which is faster.  The constant `5` is another little trick, which should
be explained in the code itself.

Anti-aliasing and supersampling
-------------------------------

Finally, when you calculate the color value of a single pixel, it is in
reality just the color of a single point in the Mandelbrot set that is
situated somewhere _inside_ that pixel.

What I'm saying is that you'll basically get pixel artifacts in the image,
especially in dense areas where the color changes (near the black set, for
instance).

So what I do is to use random sampling:  Just sample a given number of
random points inside the pixel and average the sum of the color values.
This is equivalent to rendering the plot at a higher resolution and scaling
down.

There are many supersampling techniques to use, and the random sampling was
chosen because of its simplicity.  The problem is that the resulting picture
will look a bit blurry (there are ways around this as well).

Optimizing the calculation for performance
==========================================

Calculating the Mandelbrot set is quite slow, but there are a lot of tricks
to speed it up.

When speeding up any code, the first step (after making the code _correct_,
of course) is to look at the algorithm and try to use one with a simpler
complexity class.  Unfortunately, for the Mandelbrot set, this isn't really
possible.  So the tricks mentioned here are all cases of 
_micro-optimizations_.  Nevertheless, they will improve the running time
quite a lot.

We also have to remember that we're using Javascript here, which is a
relatively slow language because of its dynamic nature.  What's interesting
in this regard, though, is to identify performance hot spots in the typical
javascript engines.  It's interesting to test the code on different browsers.

Removing the square root operation
----------------------------------

First, let's look at the inner loop.  It continually calculates the
magnitude of the complex number C, and compares this with a threshold value.
Observe that it takes the square root in doing so:

    if ( sqrt(x^2 + y^2) > threshold ) ...

If we just square the treshold value, we should be able to do away with the
square root operation:

    threshold_squared = threshold^2
    // ...
    if ( (x^2 + y^2) > threshold_squared ) ...

Taking advantage of symmetry
----------------------------

You've probably noticed that the plot is reflected vertically over the line
<span>y = 0</span>.  You can take advantage of this mirroring to halve the
computation time. I don't, because you'll mostly render plots that are
massively zoomed in.

Splitting up the main equation
------------------------------

The main equation is

> <span>z<sub>n+1</sub> = z<sub>n</sub><sup>2</sup> + c

Setting `C = z` and `Cr = Re(z)` and `Ci = Im(z)`, we get

    C_{n+1} = Cr^2 + 2Cr*Ci*i - Ci*Ci + C_{0}
    C_{n+1} = (Cr^2 - Ci^2) + i(2Cr*Ci) + C_{0}

giving us

    Re (C_{n+1}) = Cr^2 - Ci^2 + x
    Im (C_{n+1}) = 2*Cr*Ci + y
    Mag(C_{n+1}) = sqrt(Cr^2 + Ci^2)

If we introduce two variables `Tr = Cr^2` and `Ti = Ci^2`, we get

    Re (C_{n+1})   = Tr - Ti + x
    Im (C_{n+1})   = 2*Cr*Ci + y
    Mag(C_{n+1})^2 = Tr + Ti
    Tr             = Re(C_{n+1}))^2
    Ti             = Im(C_{n+1}))^2

So we have now replaced some multiplications with additions, which is
normally faster in most CPUs.  But, again, this is javascript, and
javascript has quite a different performance profile.  The code above indeed
does _not_ give us any **significant** speedup --- for a 640x480 image, we
only save a hundred milliseconds, or so.

Fast indexing into the image data struct
----------------------------------------

To plot individual pixels in HTML5 canvas, you get an array and you have to
calculate the array offset for a given coordinate pair.

I.e., given RGBA pixel format (four positions), an (x, y) coordinate pair
and a width and height, you calculate it by

    offset = 4*x + 4*y*width

so that you can now set the RGBA values as

    array[offset+0] = red
    array[offset+1] = green
    array[offset+2] = blue
    array[offset+3] = alpha

There are several ways of optimizing this.  For instance, we can simply
multiply the whole offset by four, which is the same as shifting all bits
left two positions.  However, javascript works in mysterious ways, so the
customary shift operations may not be as fast as in other languages like C
and C++.  The reason _probably_ has to do with the fact that javascript only
has _one_ data type for numbers, and my guess is that it's some kind of
float.

Anyway, we now have

    offset = (x + y*width) << 2

Another trick I'd like to mention.  Say that the width and height are fixed
to, say 640 and 480, respectively.  And old trick to multiply y by 640 would
be notice that 640 = 512 + 128 = 2^9 + 2^7, giving us

    y*640 = y*512 + y*128 = y*2^9 + y*2^7 = y<<9 + y<<7

So now we've converted one multiplication into two shifts and an add.  In
your commodity language and hardware, this might be quite fast in tight
innerloops.

Anyway, we still want to be able to use arbitrary heights and widths, so
let's skip that one.

By far, the fastest way of accessing the array is by doing it sequentially.

That is, instead of doing

    for ( y=0; y<height; ++y )
    for ( x=0; x<width; ++x ) {
      // calculate RGBA
      var offset = 4*(x + y*with);
      image.data[offset + 0] = R;
      image.data[offset + 1] = G;
      image.data[offset + 2] = B;
      image.data[offset + 3] = A;
    }

a _much_ faster way would be to do

    var offset = 0;
    for ( y=0; y<height; ++y )
    for ( x=0; x<width; ++x ) {
      image.data[offset++] = R;
      image.data[offset++] = G;
      image.data[offset++] = B;
      image.data[offset++] = A;
    }

So now we've basically saved the work of doing `2*width*height`
multiplications, or 600 thousand of them, assuming a 640x480 image.

Fast copying of the image data
------------------------------

To draw in the canvas, you request an array, update it and copy it back to
the canvas.

Of course, you want to reduce the number of such operations.  Because we
want an animation showing each line as it is drawn, we'll do this:

  * Get an image data array
  * For each line: Update the array
  * For each line: Copy the array back to the canvas

The trick here, though is to _not_ use `getImageData`.  You're going to
overwrite all existing image data, so you can use the same buffer for every
line.  So instead, we'll use these operations:

  * Get a line buffer by calling `createImageData(canvas.width, 1)`
  * For each line: Update the line buffer array
  * For each line: Call `putImageData(linebuffer, 0, y_position)` to copy only _one_ line

This ensures that we only copy _one_ line per frame update.

Embarrassingly parallel
-----------------------

Since the algorithm above is referentially transparent, meaning that it
always produces the same output for the same input (where input is defined
as `x, y, steps, threshold`), you could in theory calculate all points in
parallel.

Such algorithms are colloquially called
[embarrassingly parallel](http://en.wikipedia.org/wiki/Embarrassingly_parallel).

Now, JavaScript is inherently single-threaded:  You can only use so-called
green threads, meaning that the javascript engine will yield control between
them.

However, there is a new HTML5 APi called web workers that you can use to
create real, OS-level threads.  That should make it easy to split up
plotting into several threads (preferrably one per logical core).

Using vectorized procedures
---------------------------

The algorithm is very well suited for vector operations.  Most modern
computers come with hardware optimizations for such operations (SSE, etc).
However, we are again limited to what the javascript engines will do for us.

Even more optimizations
-----------------------

Take a look at the optimizations done to the Mandelbrot set in
[The Computer Language Benchmarks Game](http://shootout.alioth.debian.org/u32/performance.php?test=mandelbrot)

There are a lot of cool tricks going on there.  Most of _those_ use SSE
parallelism for hardware speedup or offloads to the GPU.

Future Work
-----------

Use asm.js, real threads and SIMD computations.
