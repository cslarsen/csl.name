---
layout: post
title: "Imputing the presence of HLA-B27 using your raw 23andMe data file"
date: 2015-11-12 22:18:00 +01:00
updated: 2015-11-12 22:18:00 +01:00
categories: DNA
disqus: true
tags: 23andme dna
---

<p class="lead">
Recently, I wanted to see if I could impute the presence of the <a
href="https://en.wikipedia.org/wiki/HLA-B27">HLA-B27 antigen</a> using my raw
23andMe DNA data. This is a pretty important antigen, because it is correlated
strongly with several diseases. Although I used a very small reference data
set, I managed to get a rough result that coincided with the result of a
HLA-B27 blood test (but that could have been due to luck as well).
</p>

First off, note that I'm just a hobbyist when it comes to this stuff. Don't be
afraid to let me know if this approach is wrong.if this approach is wrong.

Prerequisites
-------------

Install `snp2hla` and third party software by following the instructions at
[https://www.broadinstitute.org/mpg/snp2hla/snp2hla_manual.html](https://www.broadinstitute.org/mpg/snp2hla/snp2hla_manual.html).

I used plink 1 instead of plink2 / 1.9.
You can download the beagle version 3.0.4 from
[http://faculty.washington.edu/browning/beagle/b3.html](http://faculty.washington.edu/browning/beagle/b3.html),
or more exactly
[http://faculty.washington.edu/browning/beagle/recent.versions/beagle_3.0.4_05May09.zip](http://faculty.washington.edu/browning/beagle/recent.versions/beagle_3.0.4_05May09.zip)

To convert your 23andMe raw data file to plink output files (`.bed`, `.bim` and `.fam`),
we use plink2:

    $ plink2 --23file genome.txt SURNAME FORENAME M --out foo

Here `SURNAME` and `FORENAME` is the individual's name and must contain *no*
spaces. They're used as a marker to distinguish between people, if you want to
perform bulk operations. `M` means *male* (it can also be deduced, obviously, but may
be incorrect in some cases). Use `F` for female. The `--out foo` means plink will
create output files `foo.bed`, `foo.bim` and `foo.fam`.

Imputing the presence of HLA-B27
--------------------------------

Next, we need to perform the actual imputation for HLA. To do this, we need a
reference data set, and we'll use the `HM_CEU_REF` set. There is supposedly one
much better, with over 5000 individuals, but as of snp2hla version 1.0.3, it's not
bundled anymore, because privacy and security. If you're a serious researcher,
you can ask for a copy of the full set.

To perform imputation, simply do

    ./SNP2HLA.csh foo HM_CEU_REF foo2hla `which plink2` 2000 1000

The first argument, `foo` is the name of the output files in the previous step.
`HM_CEU_REF` is the set to base the imputation on, `foo2hla` is the output
basename for this operation (I like to discern between plink and snp2hla
output), then there's a path to plink (I use `plink2`). The last two arguments
are memory limits. It's really only needed when processing large groups of, but
I kept them anyway.

So, the output files you get now:

    foo2hla.bed
    foo2hla.bgl.gprobs
    foo2hla.bgl.log
    foo2hla.bgl.phased
    foo2hla.bgl.r2
    foo2hla.bim
    foo2hla.dosage
    foo2hla.fam

Looking in `.bgl.gprobs`, you'll find the following HLA-B27 results:

    $ grep HLA_B_27 foo2hla.bgl.gprobs
    foo2hla.bgl.gprobs:HLA_B_27 P A 0,002 0,110 0,888
    foo2hla.bgl.gprobs:HLA_B_2705 P A 0,002 0,110 0,888

The first line says that the probability for presense (`P`) is 0.002, while
it's 0.110 for absence (`A`), and the uncertainty is a whooping 0.888.  The
second line is for the `HLA-B*27:05` subtype, and it has the same
probabilities.

So, even if there is such a big uncertainty, it does give a very crude
indication, even for the small reference data set. And, it did match up with
the result of the blood test, which was fun, but could just as well have been
pure luck.

If you're a Beagle user, you can probably update me on what the other files
are, and if I'm interpreting the results correctly.

Bottom line: It's pretty awesome that ordinary people can do stuff like this.
I can easily imagine a professional service built around imputation: Patient
gets genotyped, it's stored on a secure server. The doctor can then, based on a
permission scheme, run imputation for things like HLA-B27 as *one* of many tools
when diagnosing. 

A blood test will _always_ be more accurate than imputation, but is time
consuming and costs money. If a doctor could quickly see that the probability
of HLA-B27 is low, and depending on the context, it may not be necessary to
order a blood test at all.
