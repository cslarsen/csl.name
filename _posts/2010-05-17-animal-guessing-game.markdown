---
layout: post
title:  "An animal guessing game"
subtitle: "in Python"
date:      2010-05-17 12:54:00 +00:00
updated:   2010-05-17 12:54:00 +00:00
categories: Python
disqus: true
tags: python game binary-tree data-structure
---

This is a version of the classic game of guessing which animal you
are thinking about.  It's similar to the game of *20 questions*.

The player thinks of an animal and then the program will guess which one
you're thinking about. When it runs out of questions it gives up, asking the
user to supply a question and the answer to the question. Thus, the program
builds up a [binary search tree][bst] that it will use.

It's quite a fun game, especially to code up.
This code was originally part of a guest lecture I held in 2010
at the [University of Stavanger, Norway][uis].

The objective was to have students modify this program so that it
would use pickle to save the game data to disk.

The task text was this (translated to English):

Use pickle to save questions to disk, so they won't go away when
the program exits.

Intro to pickle:

    >>> import pickle
    >>> help(pickle)

Pickle/unpickle to/from a string:

    >>> pickle.dumps("hello")
    "S'hello'\np0\n."
    >>> pickle.loads("S'hello'\np0\n.")
    'hello'

Use `pickle.dump(object, file)` to save to disk, and `pickle.load(file)`
to load from disk.

Start by saving objects to disk in the function `makeNewQuestion()`.
Next, try adding pickle code in the program startup to load the
questions from disk.  If the file does not exist, you should use
the default questions.

Here is an example game session:

    $ python animals-game.py
    Imagine an animal.  I will try to guess which one.
    You are only allowed to answer YES or NO.

    Can if fly? nope
    Were you thinking about a whale? nope
    I give up.  What did you think about? a spider
    Enter a question that would distinguish a spider from a whale: does it have
    more than two eyes?
    If I asked you this question and you thought about a spider, what would the
    correct answer be? yup
    Can if fly? no
    Does it have more than two eyes? yup
    Were you thinking about a spider? yes!!

    I knew it!
    I used 3 questions!

    Do you want to play again? yes

    Can if fly? yes
    Were you thinking about a pelican? no
    I give up.  What did you think about? a house fly
    Enter a question that would distinguish a house fly from a pelican: does it
    eat fish
    If I asked you this question and you thought about a house fly, what would
    the correct answer be? no
    Can if fly? yep, sure can!
    Does it eat fish nope
    Were you thinking about a house fly? yes

    I knew it!
    I used 3 questions!

    Do you want to play again? no

Here is the full code on GitHub:

{% gist 1574188 %}

[uis]: http://www.uis.no
[bst]: https://en.wikipedia.org/wiki/Binary_search_tree
