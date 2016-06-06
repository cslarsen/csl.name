---
layout: post
title: "Linking files with auxiliary binary data"
date: 2016-06-06 19:49:36 +0200
updated: 2016-06-06 19:49:36 +0200
categories: 
disqus: true
tags: linker
---

Programs usually load resources like images from disk. However, sometimes it's
useful to embed them directly in the executable images. The common way to do
that is to declare the bytes in the source language. E.g., in C

    const char data[] = {0x00, 0x01, ...};

A much more elegant and effective method is to have the *linker* perform this
for you. To create an object file out of `foo`, just do

    $ ld -r -b binary -o foo.o foo

That's for GNU ld. For the one that comes with OSX,


