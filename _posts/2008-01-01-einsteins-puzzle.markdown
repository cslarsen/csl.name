---
layout: post
title:  "Einstein's problem and a solution by elimination"
date:      2008-01-01 00:00:00 +00:00
updated:   2008-01-01 00:00:00 +00:00
categories: Puzzles
disqus: true
tags: puzzle logic
---

The [Zebra Puzzle][zebra] is a famous puzzle that has been said to have been
invented by Einstein. It is not. Neither is it true that only 2% of people can
solve it. But it <em>is</em> a very fun puzzle and I invite everyone to try to
solve it on their own.  Below I sketch out how I did it.

The Puzzle
----------

* There are five houses in unique colors: Blue, green, red, white and
  yellow.
* In each house lives a person of unique nationality: British, Danish,
  German, Norwegian and Swedish.
* Each person drinks a unique beverage: Beer, coffee, milk, tea and water.
* Each person smokes a unique cigar brand: Blue Master, Dunhill, Pall Mall,
  Prince and blend.
* Each person keeps a unique pet: Cats, birds, dogs, fish and horses.

The following facts are given:

1.  The Brit lives in a red house.
2.  The Swede keeps dogs as pets.
3.  The Dane drinks tea.
4.  The green house is on the left of the white, next to it.
5.  The green house owner drinks coffee.
6.  The person who smokes Pall Mall rears birds.
7.  The owner of the yellow house smokes Dunhill.
8.  The man living in the house right in the center drinks milk.
9.  The Norwegian lives in the first house.
10. The man who smokes blend lives next to the one who keeps cats.
11. The man who keeps horses lives next to the man who smokes Dunhill.
12. The owner who smokes Blue Master drinks beer.
13. The German smokes Prince.
14. The Norwegian lives next to the blue house.
15. The man who smokes blend has a neighour who drinks water.

The question you need to answer is: **Who keeps fish** ?

Try to work on this problem yourself before looking at the solution! It is
really not that hard once you find a good approach.

A Solution
----------

Given facts 8 and 9, we can safely assume that the houses are positioned in
a row. This lets us set up a table to hold all the information we find.

We will use the following codes for the various pets, cigar brands and so
on:

* **b**lue, **g**reen, **r**ed, **w**hite, **y**ellow
* **b**ritish, **d**anish, **g**erman, **n**orwegian, **s**wede
* **b**eer, **c**offee, **m**ilk, **t**ea, **w**ater
* **b**lue master, **d**unhill, **p**all mall, p**r**ince, b**l**end
* **b**irds, **c**ats, **d**ogs, **f**ish, **h**orses

Instead of starting with an empty table and filling in the possible values,
I found it much easier to just insert all possibilities up front. We will
then eliminate possible values in each cell as we read the facts. This can
easily be done with pen and paper.

<table class="solution_step">
	<tr>
		<td>Colour</td>
		<td>bgrwy</td>
		<td>bgrwy</td>
		<td>bgrwy</td>
		<td>bgrwy</td>
		<td>bgrwy</td>
	</tr>
	<tr>
		<td>Nationality</td>
		<td>bdgns</td>
		<td>bdgns</td>
		<td>bdgns</td>
		<td>bdgns</td>
		<td>bdgns</td>
	</tr>
	<tr>
		<td>Beverage</td>
		<td>bcmtw</td>
		<td>bcmtw</td>
		<td>bcmtw</td>
		<td>bcmtw</td>
		<td>bcmtw</td>
	</tr>
	<tr>
		<td>Cigar</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
	</tr>
	<tr>
		<td>Pet</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
	</tr>
</table>

First up we simply eliminate possibilities by using facts 8, 9 and 10.

<table class="solution_step">
	<tr>
		<td>Colour</td>
		<td>grwy</td>
		<td>b</td>
		<td>grwy</td>
		<td>grwy</td>
		<td>grwy</td>
	</tr>
	<tr>
		<td>Nationality</td>
		<td>n</td>
		<td>bdgs</td>
		<td>bdgs</td>
		<td>bdgs</td>
		<td>bdgs</td>
	</tr>
	<tr>
		<td>Beverage</td>
		<td>bctw</td>
		<td>bctw</td>
		<td>m</td>
		<td>bctw</td>
		<td>bctw</td>
	</tr>
	<tr>
		<td>Cigar</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
	</tr>
	<tr>
		<td>Pet</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
	</tr>
</table>

The Brit lives in a red house (fact 1), so remove colour option r from all
houses which doesnt have **b**rit as a possible nationality.

<table class="solution_step">
	<tr>
		<td>Colour</td>
		<td>gwy</td>
		<td>b</td>
		<td>grwy</td>
		<td>grwy</td>
		<td>grwy</td>
	</tr>
	<tr>
		<td>Nationality</td>
		<td>n</td>
		<td>dgs</td>
		<td>bdgs</td>
		<td>bdgs</td>
		<td>bdgs</td>
	</tr>
	<tr>
		<td>Beverage</td>
		<td>bctw</td>
		<td>bctw</td>
		<td>m</td>
		<td>bctw</td>
		<td>bctw</td>
	</tr>
	<tr>
		<td>Cigar</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
	</tr>
	<tr>
		<td>Pet</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
	</tr>
</table>

Fact 4 says the green house is on the left side, next to the white.
So the first house cannot be green or white, leaving yellow as the
only possibility.  The last house cannot be green.  By fact 5, remove
green as an option for the center house.  This house also cannot be
white, due to fact 4, leaving the colour red.  This also leads us to
conclude that the second and last houses are green and white, respectively.

We can now insert **b** for Brit in the red house.

<table class="solution_step">
	<tr>
		<td>Colour</td>
		<td>y</td>
		<td>b</td>
		<td>r</td>
		<td>g</td>
		<td>w</td>
	</tr>
	<tr>
		<td>Nationality</td>
		<td>n</td>
		<td>dgs</td>
		<td>b</td>
		<td>dgs</td>
		<td>dgs</td>
	</tr>
	<tr>
		<td>Beverage</td>
		<td>btw</td>
		<td>btw</td>
		<td>m</td>
		<td>c</td>
		<td>btw</td>
	</tr>
	<tr>
		<td>Cigar</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
		<td>bdprl</td>
	</tr>
	<tr>
		<td>Pet</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
		<td>bcdfh</td>
	</tr>
</table>

Continuing with fact 3, we remove the Dane as an option for the green house.
Also remove tea from the first house.  Fact 2 says the Swede keeps dogs, so
remove d for dogs in all houses which doesn't have **S**wede as an option.
Fact 7 says the Norwegian smokes Dunhill, and fact 10 says the next house
keeps horses.

Fact 12 says the one smoking Blue Master drinks beer, so remove b from all
houses that don't match &mdash; therefore the Norwegian must drink water.
We will also remove b for Blue Master in houses that doesn't have a beer
option.

The Swede keeps dogs, so remove s for Swede from the blue house, which has
horses.

<table class="solution_step">
	<tr>
		<td>Colour</td>
		<td>y</td>
		<td>b</td>
		<td>r</td>
		<td>g</td>
		<td>w</td>
	</tr>
	<tr>
		<td>Nationality</td>
		<td>n</td>
		<td>dg</td>
		<td>b</td>
		<td>gs</td>
		<td>dgs</td>
	</tr>
	<tr>
		<td>Beverage</td>
		<td>w</td>
		<td>bt</td>
		<td>m</td>
		<td>c</td>
		<td>bt</td>
	</tr>
	<tr>
		<td>Cigar</td>
		<td>d</td>
		<td>bprl</td>
		<td>prl</td>
		<td>prl</td>
		<td>bprl</td>
	</tr>
	<tr>
		<td>Pet</td>
		<td>bcf</td>
		<td>h</td>
		<td>bcf</td>
		<td>bcdf</td>
		<td>bcdf</td>
	</tr>
</table>

As you can see, this puzzle is quite trivial to solve once we have a good
approach.  Just keep chipping away at the problem, removing possible values
for each cell.

Fact 15 means that the blend (l) is smoked in the second house, so update
the table according to this.  Fact 6 says the first house cannot have a bird
option, as Dunhill is smoked there.  Fact 13 forces us to remove the German
option for the second house, since blend is smoked there.  Fact 3 again
gives us tea for this house, and this leaves beer for the last house.

Fact 10 means the Norwegian keeps cats.

<table class="solution_step">
	<tr>
		<td>Colour</td>
		<td>y</td>
		<td>b</td>
		<td>r</td>
		<td>g</td>
		<td>w</td>
	</tr>
	<tr>
		<td>Nationality</td>
		<td>n</td>
		<td>d</td>
		<td>b</td>
		<td>gs</td>
		<td>gs</td>
	</tr>
	<tr>
		<td>Beverage</td>
		<td>w</td>
		<td>t</td>
		<td>m</td>
		<td>c</td>
		<td>b</td>
	</tr>
	<tr>
		<td>Cigar</td>
		<td>d</td>
		<td>l</td>
		<td>pr</td>
		<td>pr</td>
		<td>bpr</td>
	</tr>
	<tr>
		<td>Pet</td>
		<td>c</td>
		<td>h</td>
		<td>bf</td>
		<td>bdf</td>
		<td>bdf</td>
	</tr>
</table>

Now everything falls into place.  We will now skip all the intermediate
steps and just show you the final table.

<table class="solution_step">
	<tr>
		<td>Colour</td>
		<td>y</td>
		<td>b</td>
		<td>r</td>
		<td>g</td>
		<td>w</td>
	</tr>
	<tr>
		<td>Nationality</td>
		<td>n</td>
		<td>d</td>
		<td>b</td>
		<td>g</td>
		<td>s</td>
	</tr>
	<tr>
		<td>Beverage</td>
		<td>w</td>
		<td>t</td>
		<td>m</td>
		<td>c</td>
		<td>b</td>
	</tr>
	<tr>
		<td>Cigar</td>
		<td>d</td>
		<td>l</td>
		<td>p</td>
		<td>r</td>
		<td>b</td>
	</tr>
	<tr>
		<td>Pet</td>
		<td>c</td>
		<td>h</td>
		<td>b</td>
		<td>f</td>
		<td>d</td>
	</tr>
</table>

[zebra]: https://en.wikipedia.org/wiki/Zebra_Puzzle
