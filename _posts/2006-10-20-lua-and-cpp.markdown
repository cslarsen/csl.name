---
layout: post
title:  "Using Lua with C++"
subtitle: "A short tutorial"
date:     2006-10-20 21:42:24 +02:00
updated:  2016-05-11 00:14:00 +00:00
categories: Lua
tags: Lua C++
description: "Tutorial on creating Lua host programs in C++"
keywords: "Lua C++ Tutorial script programming language C"
disqus: true
---

In this short tutorial I'll show how to run Lua programs from C and C++ and how
to expose functions to them. It's easy!

**Update:** The code in this post has been updated for Lua 5.2.4. I haven't
checked if the Lua 5.3 C API is backwards-compatible with 5.2. All the code
here is available <a href="https://github.com/cslarsen/lua-cpp">on GitHub</a>.

The first program will just create a Lua state object and exit. It will be a
hybrid between C and C++. Since the two languages must include different files,
we need to discern between them by checking for the existence of the
`__cplusplus` macro.

    #ifdef __cplusplus
    # include <lua5.2/lua.hpp>
    #else
    # include <lua5.2/lua.h>
    # include <lua5.2/lualib.h>
    # include <lua5.2/lauxlib.h>
    #endif

    int main()
    {
      lua_State *state = luaL_newstate();
      lua_close(state);
      return 0;
    }

Notice that I'm being explicit about which version of Lua I'm using in the
code. If you trust that the Lua developers care about compatibility, you can
just `#include <lua.hpp>` and so on directly.

The purpose of the program is just to make sure that we can compile, link and
run it without errors.

You need to let the compiler know where it can find the include files and the
Lua shared library. The include files are usually located in
`/usr/local/include` and the library files in `/usr/local/lib`. Search your
system directories if needed.  To compile the above program, pass the
directories with `-I` and `-L`, respectively.

    $ g++ -W -Wall -g -o first first.cpp \
        -I/usr/local/include -L/usr/local/lib -llua

You may swap out `g++` with `llvm-g++`, or just `c++`, depending on your
compiler. If you're using a C compiler, use `gcc` or `llvm-gcc` — but
remember to rename the file to `first.c`.

Now try to run the program to make sure it doesn't segfault:

    $ ./first
    $ echo $?
    0

This one worked just fine.

Executing Lua programs from a host
----------------------------------

The next step is to execute Lua programs from your C or C++ code. We'll create
the Lua state object as above, load a file from disk and execute it.

Put this into `runlua.cpp` or `runlua.c`:

    #include <stdio.h>

    #ifdef __cplusplus
    # include <lua5.2/lua.hpp>
    #else
    # include <lua5.2/lua.h>
    # include <lua5.2/lualib.h>
    # include <lua5.2/lauxlib.h>
    #endif

    void print_error(lua_State* state) {
      // The error message is on top of the stack.
      // Fetch it, print it and then pop it off the stack.
      const char* message = lua_tostring(state, -1);
      puts(message);
      lua_pop(state, 1);
    }

    void execute(const char* filename)
    {
      lua_State *state = luaL_newstate();

      // Make standard libraries available in the Lua object
      luaL_openlibs(state);

      int result;

      // Load the program; this supports both source code and bytecode files.
      result = luaL_loadfile(state, filename);

      if ( result != LUA_OK ) {
        print_error(state);
        return;
      }

      // Finally, execute the program by calling into it.
      // Change the arguments if you're not running vanilla Lua code.

      result = lua_pcall(state, 0, LUA_MULTRET, 0);

      if ( result != LUA_OK ) {
        print_error(state);
        return;
      }
    }

    int main(int argc, char** argv)
    {
      if ( argc <= 1 ) {
        puts("Usage: runlua file(s)");
        puts("Loads and executes Lua programs.");
        return 1;
      }

      // Execute all programs on the command line
      for ( int n=1; n<argc; ++n ) {
        execute(argv[n]);
      }

      return 0;
    }

You can reuse the compilation arguments from above:

    $ g++ -W -Wall -g -I/usr/local/include \
        -L/usr/local/lib -llua runlua.cpp -o runlua

or

    $ gcc -W -Wall -g -I/usr/local/include \
        -L/usr/local/lib -llua runlua.c -o runlua

Running Lua programs
--------------------

Let's test this with some Lua programs. The first one prints the Lua version
and exits.

    io.write(string.format("Hello from %s\n", _VERSION))

You may want to double-check that it works by running `lua hello.lua`. It may
not be important for this trivial program, but can become important when you
try more advanced ones.

    $ lua lua/hello.lua
    Hello from Lua 5.2

Now try it with `runlua`:

    $ ./runlua lua/hello.lua
    Hello from Lua 5.2

You can even run bytecode-compiled programs:

    $ luac -o lua/hello.luac lua/hello.lua
    $ ./runlua lua/hello.luac
    Hello from Lua 5.2

We should also check that the error handling works. Put some garbage in a file
called `error.lua`, for example

    This file is not a Lua program.

Running it produces

    $ ./runlua lua/error.lua
    lua/error.lua:1: syntax error near 'is'

Calling C functions from Lua
----------------------------

It gets very interesting when Lua programs call back to your C or C++
functions. We'll create a function called `howdy` that prints its input
arguments and returns the integer 123.

To be on the safe side, we'll declare C linkage for the function in the C++
version of the program. This has to do with <a
href="https://en.wikipedia.org/wiki/Name_mangling#C.2B.2B">name mangling</a>,
but in this case, it really doesn't matter: Lua just receives a pointer to a
function, and that's that. But if you start using dynamic loading of shared
libraries through `dlopen` and `dlsym`, this will be an issue. So let's do it
correct from the start.

Copy the above program into a file called `callback.cpp` and add the `howdy`
function.

    #ifdef __cplusplus
    extern "C"
    #endif
    int howdy(lua_State* state)
    {
      // The number of function arguments will be on top of the stack.
      int args = lua_gettop(state);

      printf("howdy() was called with %d arguments:\n", args);

      for ( int n=1; n<=args; ++n) {
        printf("  argument %d: '%s'\n", n, lua_tostring(state, n));
      }

      // Push the return value on top of the stack. NOTE: We haven't popped the
      // input arguments to our function. To be honest, I haven't checked if we
      // must, but at least in stack machines like the JVM, the stack will be
      // cleaned between each function call.

      lua_pushnumber(state, 123);

      // Let Lua know how many return values we've passed
      return 1;
    }

We have to pass the address of this function to Lua along with a name. Put the
following line somewhere between the call to `lua_newstate` and
`luaL_loadfile`:

    // Make howdy() available to Lua programs under the same name.
    lua_register(state, "howdy", howdy);

Create a test program called `callback.lua`

    io.write("Calling howdy() ...\n")
    local value = howdy("First", "Second", 112233)
    io.write(string.format("howdy() returned: %s\n", tostring(value)))

Compile and test it

    $ g++ -W -Wall -g -I/usr/local/include -L/usr/local/lib \
        -llua  callback.cpp -o callback
    $ ./callback lua/callback.lua
    Calling howdy() ...
    howdy() was called with 3 arguments:
      argument 1: 'First'
      argument 2: 'Second'
      argument 3: '112233'
    howdy() returned: 123

I told you it was easy!

What next?
----------

Read the <a href="https://www.lua.org/manual/5.2/manual.html#4">Lua C API
Reference</a>. You've learned enough now to get going with it. Did you see my
note about clearing the stack in `howdy`? You may want to investigate that.

Find out how to integrate Lua closures with your C functions.

If you want to hide or catch console output from Lua, you need to figure that
out as well. I once did it by trapping `io.write()`; I copied its code from
`lualib.c` and changed `io_write` to point to my own function. There is
probably a better way to do it, though. Doing so is useful for things like game
programming.

Use <a
href="https://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization">RAII</a>
or smart pointers to manage resources like `lua_State`.

I also strongly recommend to try out <a href="http://luajit.org">LuaJIT</a>.
Calling into your functions there is even easier, using LuaJIT's foreign
function library. I'll write a blog post on how to do that as well. In short,
just create ordinary C functions, compile as a shared libraries, copy their
signatures into pure Lua source code and hook them up with <a
href="http://luajit.org/ext_ffi_tutorial.html">LuaJIT's FFI library</a>.

LuaJIT runs between 10-20 and up to 135 times faster than interpreted Lua, so
it's definitely worth it.
