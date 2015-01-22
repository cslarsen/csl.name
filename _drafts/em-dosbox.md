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

Now, you can find `dosbox.js` and `dosbox.html` in `src/`.  These are templates
for the web page that you'll use to run MS-DOS programs.

You can try creating a new page for some DOS program like so:

    $ cd src
    $ ./packager.py test TEST.EXE

On my system, it complains that it can't find `file_packager.py`.  This is part
of the emscripten package. With Homebrew, emscripten is symlinked to the Cellar
directory.  You should therefore set `EMSCRIPTEN_ROOT` in `~/.emscripten` to
point to where the files are actually located.

Check what value `EMSCRIPTEN_ROOT` is:

    $ em-config EMSCRIPTEN_ROOT

On my system I had to set this to
`/usr/local/Cellar/emscripten/1.28.2/libexec`.

Now, I have an old intro I made back in the day, called `intro.exe`.  Let's try
to bundle it with em-dosbox:

    $ cd em-dosbox/src
    $ ./packager.py intro intro.exe

Because of the same-origin policy, you need to test this through a simple web server:

    $ python -m SimpleHTTPServer &
    $ open http://localhost:8000/intro.html

If everything worked, you'll see your DOS program running! At least it worked for me!

You may have to tweak some settings, though.  For instance, for my particular
intro, I had to change some settings.

