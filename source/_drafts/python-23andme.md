---
layout: post
title: "Parsing your 23andMe genome with Python"
date: 2017-11-20 10:39:25 +0100
updated: 2017-11-20 10:39:25 +0100
categories: python 23andme
disqus: true
tags: python dna 23andme
---

This post shows how to parse and interpret your raw genome data from
[23andMe][23andme]. We will do so in Python, using a [Arv, a Python genome
parser ][arv] that I wrote. With it, you'll be able to do things like

    import arv

    genome = arv.load(filename)

    male = genome.y_chromosome == True

    complexion = arv.unphased_match(genome["rs1426654"], {
        "AA": "light",
        "AG": "mixed",
        "GG": "dark"})

    eye_color = arv.unphased_match(genome["rs12913832"], {
        "AA": "brown",
        "AG": "brown or green",
        "GG": "blue"})

    print("You are a {gender} with {color} eyes and {complexion} skin.".format(
      gender = "man" if male else "woman,
      complexion = complexion,
      color = eye_color))

In my case, this program will print

    You are a man
    Your skin is light
    You have blue eyes

[23andMe][23andme] is a company that sequences your personal genome from a
personal saliva sample. With it, you can delve into your genomic ancestry,
discover traits and health issues and more. They used to provide such reports
on their web site, but was forced to remove them by the FCC. But you can still
download your raw genome data in CSV format.
data.

Disclaimer
----------

Before continuing, it is important to note that I am just a hobbyist. This
article may contain errors. Also, parsing genome data may be unlawful in some
countries. Usually, it's okay if you only parse your personal data, for
educational purposes. If you're going to try this yourself, you do it at
your own risk. Do not rely on the results here — always consult a medical
doctor.

Installing the parser
---------------------

You need the [Arv][arv] module. It works on Python 2 and 3 and is installable
via PyPi:

    $ pip install arv

Of course, you can also parse the CSV file yourself, but Arv is insanely fast
and contains functionality for inferring from the genome.
It parses such a file in 0.06 seconds.

Contents of the raw genome data
-------------------------------

Genome wide association studies
-------------------------------



[23andme]: https://www.23andme.com
[arv]: https://github.com/cslarsen/arv
