---
layout: post
title: "Writing a shared C++ library and loading it in LuaJIT"
date: 2016-05-15 05:25:47 +0000
updated: 2016-05-15 05:35:47 +0000
categories: lua programming
disqus: true
tags: lua jit c
---

This tutorial shows how to create a shared library in C and C++, and how to
load it in <a href="http://luajit.org">LuaJIT</a> using its foreign-function interface (FFI).
The code here is <a href="https://github.com/cslarsen/luajit-cpp">available on GitHub</a>.

There are obvious upsides to using LuaJIT: It's considerably faster than plain
Lua — up to <a href="http://luajit.org/performance.html">a hundred times
faster</a> —  and it comes with a really nice way of loading shared libraries
through its FFI library.

First, let's start off by creating a <a
href="http://www.oracle.com/technetwork/articles/servers-storage-dev/mixingcandcpluspluscode-305840.html">hybrid
C and C++ shared library</a>. That means you can either use a C or C++ compiler
to build it.

Put the following in a file called `first.cpp` or `first.c`.

    #ifdef __cplusplus
    extern "C"
    #endif
    int add(int a, int b)
    {
      return a + b;
    }

To compile it, pass `-fPIC` to generate position independent code, `-shared` to
produce a shared library and set the output file to `libfirst.so` (or
`libfirst.dylib  for OS X).

    $ g++ -W -Wall -g -fPIC -shared -o libfirst.so first.cpp

The C version of the program will be

    $ gcc -W -Wall -g -fPIC -shared -o libfirst.so first.c

Now that we've made `libfoo.so`, we can inspect it.

    $ file libfirst.so
    libfirst.so: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV),
    dynamically linked, not stripped

We can list its symbols

    $ nm --defined-only libfirst.so | grep add
    000000000000052a T add

If we remove the debugging symbols for `libfirst.so`, we can't use `nm`, but we
can use `objdump`.

    $ strip libfirst.so
    $ objdump -T libfirst.so

    libfirst.so:     file format elf64-x86-64

    DYNAMIC SYMBOL TABLE:
    0000000000000420 l    d  .init  0000000000000000              .init
    ...
    0000000000200808 g    D  .bss   0000000000000000  Base        _end
    ...

We also know the signature of the function `add`, namely

    int add(int, int)

That's really all we need to load a shared library using LuaJIT. Let's try it
out in the REPL first.

    $ luajit
    > ffi = require("ffi")
    > first = ffi.load("libfirst.so")

We not have to lef `ffi` know input and output arguments for the function
`add`, by simply passing the function signature to `ffi.cdef`.

    > ffi.cdef("int add(int, int);")

Now we can call `add`:

    > io.write(first.add(11, 22) .. "\n")
    33

That's how easy it is. For Python users, there is the `cffi` module that comes
with `pypy`, which offers similar functionality (unlike `ctypes`, which doesn't
parse C signatures automatically). Many other languages have it too. Chicken
Scheme also parses C code. The difference is in how advanced the C parsers are.

But why would you interface with C? I think it's very obvious. Most interesting
platform specific functions will be available in C. Having a good FFI library
means you can easily access these in LuaJIT. It's also easy to interface with
libraries such as <a href="http://www.libsdl.org">SDL</a>.

Using C++ objects in LuaJIT
---------------------------

The next step is to use C++ objects. There are many ways of doing this, so I'll
focus on one. We'll create a C++ class in a file `foo.cpp`, then we'll expose
it through a C interface that we'll use from LuaJIT. Finally, we'll wrap this
interface back into an object-like one in LuaJIT.

Below is `foo.cpp`.

    class Person {
    public:
      Person(const std::string& name_,
             const int age_):
        name(name_),
        age(age_)
      {
      }

      const std::string name;
      const int age;
    };

The C interface will look like this:

    extern "C" void* new_person(const char* name, int age)
    {
      assert(name != NULL);
      Person *p = new Person(name, age);
      return reinterpret_cast<void*>(p);
    }

    extern "C" void delete_person(Person* p)
    {
      delete p;
    }

    extern "C" int age(const Person* p)
    {
      assert(p != NULL);
      return p->age;
    }

    extern "C" char* name(const Person* p)
    {
      assert(p != NULL);
      return strdup(p->name.c_str());
    }

Note that the `strdup` function in `name` is not guaranteed to exist on your
system. You can roll your own with something like

    static char* strdup(const char* in)
    {
      assert(s != NULL);
      char *out = (char*) malloc(strlen(in)+1);
      return strcpy(out, in);
    }

We need to tell LuaJIT's FFI library what the function signatures are.

    ffi.cdef[[
      /* From our library */
      typedef struct Person Person;
      Person* new_person(const char* name, const int age);
      char* name(const Person* p);
      int age(const Person* p);

      /* From the C library */
      void free(void*);
    ]]

We'll wrap this up in an <a
href="http://lua-users.org/lists/lua-l/2011-07/msg00496.html">object-like
structure using a trick</a>.

    local PersonWrapper = {}
    PersonWrapper.__index = PersonWrapper

    local function Person(...)
      local self = {super = foo.new_person(...)}
      ffi.gc(self.super, foo.delete_person)
      return setmetatable(self, PersonWrapper)
    end

Notice that we pass the pointer to `ffi.gc`, which makes sure to call
`foo.delete_person` on the pointer reclaiming it.

For the `name` function, we get a newly allocated string using `malloc`, so we
need to use `C.free` to remove it from the heap again. To load the C library,

    C = ffi.C

We can now implement the wrappers for `name` and `age`.

    function PersonWrapper.name(self)
      local name = foo.name(self.super)
      ffi.gc(name, C.free)
      return ffi.string(name)
    end

    function PersonWrapper.age(self)
      return foo.age(self.super)
    end

Finally, let's try it out:

    local ffi = ffi.require("ffi")
    local foo = ffi.load("libfoo.so")

    -- Insert the above code here

    local person = Person("Mark Twain", 74)
    io.write(string.format("'%s' is %d years old\n",
                           person:name(),
                           person:age()))

Running it produces

    $ luajit foo.lua
    'Mark Twain' is 74 years old

