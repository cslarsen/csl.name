---
layout: post
title:  "HLA allele analysis of 23andMe raw data"
date:    2015-11-12 22:18:00 +01:00
categories: DNA
disqus: true
tags: plink 23andMe
---

<p class="lead">
Here are instructions on how to run SNP2HLA for 23andMe raw data.
</p>

Install snp2hla and third party software by following the instructions at:

    https://www.broadinstitute.org/mpg/snp2hla/snp2hla_manual.html

I used plink 1 instead of plink2 / 1.9.
You can download the beagle version 3.0.4 from

    http://faculty.washington.edu/browning/beagle/b3.html

or more exactly

    http://faculty.washington.edu/browning/beagle/recent.versions/beagle_3.0.4_05May09.zip

To convert 23andMe file to plink output files (.bed, .bim and .fam),
we use plink2:

    plink2 --23file genome.txt SURNAME FORENAME M --out foo

Here "surname" is the surname of the individual, must contain NO spaces.
Forename is the first name. M means male (it can also be deduced, but that may
be incorrect in some cases). Use F for female. The --out foo means plink will
create output files foo.bed, foo.bim and foo.fam

NOTE: Check if the file format has changed between plink 1.07 and 1.9, if so,
that may be the reason this is not working. We use plink 1.07 later on for
snp2hla, because it seems to require it (actually, just try to use plink2 there
as well)....

TODO: Try the --snps-only no-DI ?? to remove indels? I don't think this is a
problem, though..

Next, we need to perform the actual imputation for HLA. To do this, we need a
reference data set, and we'll use the `HM_CEU_REF` set. There is supposedly one
much better with over 5000 individuals, but as of v1.0.3 of snp2hla, it's not
bundled anymore because of security. Don't know what they mean, either if the
output will be too good, or because they don't want to copy around people's
genomes (from the dataset) ? Anyway, I don't THINK it should be a problem to
use the one we have......

So, next step is to impute, we do that with

    ./SNP2HLA.csh foo HM_CEU_REF foo2hla `which plink2` 2000 1000

So, first arg is the name of the output files in step one, but without
extension. The next is the data set to use to impute, then another output name
(we just reuse foo, i dont think it will overwrite any of the existing files)..
then the path to plink, we will use plink2 actually.. then there are some
optional memory limits, in case you're doing this for many many individuals (in
which case you don't want to crash with out of memory).

... hmm good, it seems to take MUCH more time this time around... so I hope
it's working...

So, the output files you get now:

    foo2hla.bed
    foo2hla.bgl.gprobs
    foo2hla.bgl.log
    foo2hla.bgl.phased
    foo2hla.bgl.r2
    foo2hla.bim
    foo2hla.dosage
    foo2hla.fam

I'm not sure what all of these do, but I THINK the .bgl.phased is actually a
phased genome! Which means you can tell which of AG came from each parent. This
is deduced from the `HM_CEU_REF` set, and may not be accurate...

The important file is the foo2hla.dosage file, which we will use next to find
out which HLA variants the individual has.

PROBLEM: Almost all of the numbers on the end of the line is 0.000...

BUT, i think this is only because it means that it's missing.. im testing on an
individual with the new 23andme chip, which has MUCH less data...

Last step is to run the association.. Here I'll run plink2 again:

    plink2 --dosage foo2hla.dosage noheader format=1 --fam foo2hla.fam
    --logistic --out foo2assoc

Problem is, I get

    Error: All people removed by automatic --prune.

And the --prune command says

    --prune          : Remove samples with missing phenotypes.

So... I guess there are some missing phenotypes... which is weird, does it mean
it didn't generate any phenotype for the previous files. SHOULD it really do
that? I don't know.

I can use plink1 at this point, which works: Same as above, but with plink
instead of plink2.

Hmm, it says "Assuming a disease phenotype (1=unaff, 2=aff, 0=miss)" for
plink1... is there an option I've missed? I'm supposed to be able to specify a
phenotype file with --pheno, which is a .phe file.

HMMM... I don't think we need the last comand, that's only for association
studies.. so if you want to find out if some disease or condition has a
potential link to the HLA antigen, you can do the association. Probably, beagle
is enough to find the imputation..?

OOLD:
=====

Yep, so now you have the bed/bim/fam plink files files... let's try snp2hla:

    ./SNP2HLA.csh siri HM_CEU_REF 1958BC_IMPUTED plink 2000 1000

no wait, did this to get the correct output filename:
(the above created `19588c_imputed` files)

    ./SNP2HLA.csh siri HM_CEU_REF siri plink

This gives a dosage file... Wow, also seems it phases the snps too!!!!
cool! this means that if determines for each snp which of the markers comes
from which parent (e.g. the trait FOO you have came from mum, etc)

Dont really know what the other stuff really means, but posterier probabilities
i guess is the probs BEFORE actually finding out further..?

Now running:

    plink --noweb --dosage siri.dosage noheader format=1 --fam siri.fam
    --logistic --out siri2.assoc


