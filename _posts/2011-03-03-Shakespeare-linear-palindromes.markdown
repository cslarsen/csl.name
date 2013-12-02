---
layout: post
title:  "Palindromes in The Gettysburg Address and Shakespeare&apos;s collected works"
formatted_title:  "<span class='lighter'>Palindromes in <span class='bolder'>The Gettysburg Address</span> and <span class='bolder'>Shakespeare&apos;s</span> collected works</span>"
subtitle: "A programming puzzle"
date:   2011-03-03 09:10:27 -08:00
categories: C
tags: C palindrome puzzle
---

Some years ago, I [solved some programming puzzles][gist] posted by Greplin (later
Cue, then acquired by Apple).

The task was to find the longest [palindrome][palindrome]&mdash;words or
sentences that read the same forwards and backwards&mdash;in Abraham
Lincoln's [Gettysburg Address][gettysburg].

Of course, the code had to be as fast and elegant as possible.  (Correct
submissions lead to recruitment talks. I did talk with them, but as a
foreigner I did not have any US worker's visa, and they were too small to
have any time and money to spend on getting me one.)

Anyway, there are many algorithms for doing this. Mine is just based on a
brute force scan of the text. I see I claim that the running time is linear
on the average input, but quadratic in the worst case (such as a file with
all `a`s). Since writing it, I haven't double&dash;checked the running time
analysis.

{% highlight C %}
/*
 * Find the longest palindrome in the text.
 *
 * This is Greplin's first challenge, and I originally solved it in Python.
 *
 * This algorithm is linear on the average input, but has a quadratic
 * worst case running time.  There exists an even better algorithm, but
 * this should do.
 *
 * There might also be a small error below, but you get the general idea.
 *
 * Christian Stigen Larsen
 */

#include <stdio.h>

static const char text[] =
  "Fourscoreandsevenyearsagoourfaathersbroughtforthonthiscontainentanewnati"
  "onconceivedinzLibertyanddedicatedtothepropositionthatallmenarecreatedequ"
  "alNowweareengagedinagreahtcivilwartestingwhetherthatnaptionoranynartions"
  "oconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzha"
  "twarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthose"
  "whoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandpropert"
  "hatweshoulddothisButinalargersensewecannotdedicatewecannotconsecrateweca"
  "nnothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecr"
  "ateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenor"
  "longrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthel"
  "ivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughthereha"
  "vethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafsk"
  "remainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatca"
  "useforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvet"
  "hatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbi"
  "rthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotp"
  "erishfromtheearth";

void print_best(const char* l, const char* r)
{
  static int best = 2;

  if ( r-l > best )
    printf("%.*s\n", (best=r-l), l);
}

int main()
{
  const char *l, *r;

  for ( const char *p = text + 1; *p; ++p ) {
    l = p; r = p - 1;
    while ( *--l == *++r );

    print_best(l+1, r);

    l = r = p;
    while ( *--l == *++r );

    print_best(l+1, r);
  }

  return 0;
}
{% endhighlight %}

The output is

{% highlight bash %}
$ gcc -O3 -W -Wall gettysburg.c -ogetty && time ./getty
eve
ranynar

real  0m0.004s
user  0m0.001s
sys   0m0.002s
{% endhighlight %}

So the longest palindrome in The Gettysburg Address is <i>ranynar</i>.

The above code is quite fast as well. It runs through the complete works of
Shakespeare in about 0.03 seconds of wall clock time, or a processing rate
of 128 Mb/second, and grows linearly with the input. It has a worst case
running time which is quadratic, though; if you feed it a string of same
characters, for instance.

To find the palindromes in Shakespeare's collected works, I first modified
the code to load the text from disk. I changed print_best to print all
palindromes equal to or longer than the current best, so we get a longer
list of palindromes. I also had to prepare Shakespeare's collected works
into a format suitable for processing: First I removed the name of which
character is speaking (done by sed), then I converted all text to lowercase
and deleted all non-alphanumeric characters.

Since Shakespeare wrote his plays more than 300 years before copyright law
was invented, you can download and use it freely. I got mine off a site with
all the stuff in different directories. Here's what I did to prepare them,
all in one jolly line of unix goodness:

{% highlight bash %}
$ find shakespeare/ -type f \        # for all files
    | xargs sed 's/^[A-Za-z]*//g' \  # only keep alphabetical chars
    | tr A-Z a-z \                   # translate to lowercase
    | tr -dC a-z \                   # remove anything BUT alphabetical chars
    > shakespeare.txt                # output to one big file
{% endhighlight %}

I compiled my code and ran it on the file:

{% highlight bash %}
$ llvm-g++ -O4 -flto pali.cpp -o pali
$ cat shakespeare.txt | time ./pali
ll
ll
ll
ama
lonanol
tomymot
withtiw
iwerewi
erewere
tarorat
rownwor
sieveis
tomymot
imadami
madammadam
ereherehere
reherehereher
hereherehereh
illitmadamtilli
madammadammadam
madammadammadam

real    0m0.026s
user    0m0.022s
sys     0m0.004s
{% endhighlight %}

The longest palindromes appear in the lines:

<p class="quote">
GLOUCESTER<br/>
<br/>
So w<span class="pink">ill it madam till I</span> lie with you.
</p>

from the play [Richard III][rich3]. The preparsing of the collected works
above includes the character's name. Without it, the output is a bit
different:

{% highlight bash %}
$ find shakespeare/ -type f \
    | xargs cat \
    | sed -e 's/^[A-Za-z]*//g' \
    | tr A-Z a-z \
    | tr -dC a-z > collected-works-no-names.txt

$ cat collected-works-no-names.txt | ./pali
ama
lonanol
nymedemyn
ereherehere
reherehereher
illitmadamtilli
{% endhighlight %}

The second last character-by-character palindrome comes from the tragedy
[Troilus and Cressida][cressida]:

<p class="quote">
PANDARUS<br/>
<br/>
Hark! they are coming from the field: shall we
stand up here, and see them as they pass toward
Ilium? good niece, do, sweet niece Cressida.<br/>
<br/>
CRESSIDA<br/>
<br/>
At your pleasu<span class="pink">re</span>.<br/>
<br/>
PANDARUS<br/>
<br/>
<span class="pink">Here, here, her</span>e's an excellent place; here we may
see most bravely: I'll tell you them all by their
names as they pass by; but mark Troilus above the rest.
</p>

You can see it ignores a punctuation mark, which I think is fine.

There are probably other, much better algorithms for finding palindromes. If
you're interested in <i>generating</i> palindromes, you should check out
[Peter Norvig's supposed world record palindrome][norvig].

[gist]: https://gist.github.com/cslarsen/851611
[palindrome]: https://en.wikipedia.org/wiki/Palindrome
[gettysburg]: https://en.wikipedia.org/wiki/Gettysburg_Address
[norvig]: http://norvig.com/palindrome.html
[cressida]: https://en.wikipedia.org/wiki/Troilus_and_Cressida
[rich3]: https://en.wikipedia.org/wiki/Richard_III_(play)
