---
layout: post
title:  "Compiling em-dosbox on Mac OS X"
date:    2015-01-22 19:00:00 +01:00
updated: 2015-01-22 19:00:00 +01:00
categories: Dosbox
disqus: true
tags: emscripten
---

{% lead %}
[Archive.org](https://archive.org/details/softwarelibrary_msdos_games/v2) has
released a number of MS-DOS games that can be played in the browser using
em-dosbox.

Here I'll show you how you can put your own MS-DOS stuff on the web by building em-dosbox.
I'll be using Mac OS X and Homebrew.
{% endlead %}

Installing Emscripten
---------------------

Before installing emscripten, review if you want to build it with any
particular settings:

    $ brew options emscripten

I did

    $ brew install emscripten --with-closure-compiler

Homebrew tells you that you need to update your `~/.emscripten` after running
`emcc` for the first time:

    Manually set LLVM_ROOT to
      /usr/local/opt/emscripten/libexec/llvm/bin

So let's run emcc first.

    $ emcc
    WARNING  root: (Emscripten: system change: 1.28.2|asmjs-unknown-emscripten||6.0 vs 1.4.7|le32-unknown-nacl, clearing cache)
    WARNING  root: LLVM version appears incorrect (seeing "6.0", expected "3.4")
    WARNING  root: could not check fastcomp: [Errno 2] No such file or directory
    INFO     root: (Emscripten: Running sanity checks)
    CRITICAL root: Cannot find clang++, check the paths in ~/.emscripten

Now edit `~/.emscripten` and comment out the line with `LLVM_ROOT`. Set it to
what Homebrew suggests:

    LLVM_ROOT = "/usr/local/opt/emscripten/libexec/llvm/bin"

Now try running `emcc` to see that it runs without any errors.

    $ emcc
    WARNING  root: (Emscripten: settings file has changed, clearing cache)
    INFO     root: (Emscripten: Running sanity checks)
    WARNING  root: no input files

Looks good.

Installing em-dosbox
--------------------

Now you need the `em-dosbox` sources. After that, you need to run `autogen.sh`.
If it fails, you probably need to install autotools.

  $ git clone em-dosbox....
  $ cd em-dosbox
  $ ./autogen.sh

That should create a `configure` file.  Now use `emconfigure`:

  $ emconfigure ./configure

If that works fine, you should be able to build em-dosbox:

  $ make -j4


