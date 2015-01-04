---
layout: post
title:  "Calling C functions from Python"
subtitle: ""
date:      2006-03-28 00:00:00 +00:00
updated:   2006-03-28 00:00:00 +00:00
categories: Python
disqus: true
tags: Python C UNIX
description: How to call C code from Python
---

{% lead %}
Here's a small tutorial on how to call your C functions from Python.
{% endlead %}

Let's make some simple functions in C. We'll call the file `myModule.c`.

{% highlight C %}
#include <Python.h>

/*
 * Function to be called from Python
 */
static PyObject* py_myFunction(PyObject* self, PyObject* args)
{
  char *s = "Hello from C!";
  return Py_BuildValue("s", s);
}

/*
 * Another function to be called from Python
 */
static PyObject* py_myOtherFunction(PyObject* self, PyObject* args)
{
  double x, y;
  PyArg_ParseTuple(args, "dd", &x, &y);
  return Py_BuildValue("d", x*y);
}

/*
 * Bind Python function names to our C functions
 */
static PyMethodDef myModule_methods[] = {
  {"myFunction", py_myFunction, METH_VARARGS},
  {"myOtherFunction", py_myOtherFunction, METH_VARARGS},
  {NULL, NULL}
};

/*
 * Python calls this to let us initialize our module
 */
void initmyModule()
{
  (void) Py_InitModule("myModule", myModule_methods);
}
{% endhighlight %}

Compiling dynamic libraries on Mac OS X is different from the usual gcc -shared
you might be used to:

    gcc -dynamiclib -I/usr/include/python2.3/ -lpython2.3 -o myModule.dylib myModule.c

Now you have to do something awkward; rename myModule.dylib to myModule.so, so
that Python will find the correct file (this is a bug in Python, it should've
been fixed, but that's as far as I know):

    mv myModule.dylib myModule.so

If you are using a system that supports -shared you can simply do this:

    gcc -shared -I/usr/include/python2.3/ -lpython2.3 -o myModule.so myModule.c

On Windows you can reportedly type

    gcc -shared -IC:\Python27\include -LC:\Python27\libs myModule.c -lpython27 -o myModule.pyd

Here's a simple program in Python to call your functions:

{% highlight Python %}
from myModule import *

print "Result from myFunction:", myFunction()
print "Result from myOtherFunction(4.0, 5.0):", myOtherFunction(4.0, 5.0)
{% endhighlight %}

The output is:

    Result from myFunction(): Hello from C!
    Result from myOtherFunction(4.0, 5.0): 20.0

If you are going to make bigger libraries available from Python I *strongly*
suggest you check out [SWIG][swig] or [Boost Python][boost.python].

[boost.python]: http://www.boost.org/doc/libs/1_57_0/libs/python/doc/index.html
[swig]: http://www.swig.org
