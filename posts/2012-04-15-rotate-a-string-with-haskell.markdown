---
title: "Rotate A String with Haskell"
tags: haskell
---

I qualified for [Google Code Jam](http://code.google.com/codejam) this year
answering 3 out of 4 questions. I have no illusions about progressing very far,
but I'm using it as an excuse to learn some Haskell and to write code faster.

As part of the learning process, I took one of my correct solutions over to the
nice folks at the #haskell channel over on freenode and asked if there was a
better way to do it. I learned something surprising.

<!-- more -->

As part of the solution to the
[Recycled Numbers](http://code.google.com/codejam/contest/1460488/dashboard#s=p2)
problem required turning a number such as 1234 into a list of numbers with the
digits rotated [4123, 3412, 2341]. User TSC2 in the channel gave me this simple
way of doing it:

``` bash ghci console
ghci> (\xs -> let n = length xs in (tail . take n . map (take n) . tails . cycle) xs) "abcde"
["bcdea","cdeab","deabc","eabcd"]
```

I'm still new to this so I had to work it out on paper, but here is a step by
step explanation of what is happening: working from right to left as the
functions are evaluated:

{% codeblock xs %}
"abcde"
{% endcodeblock %}

`xs` is just the input string.

{% codeblock cycle %}
"abcdeabcde..."
{% endcodeblock %}

`cycle` creates an infinite list out of the input list by repeating it. Haskell
is lazy. It doesn't actually create the list until it actually needs it, so I've
added the ellpises just to show the concept that this is repeated infinitely.

{% codeblock tails %}
["abcdeabcde...", "bcdeabcdea...", "cdeabcdeab...", "deabcdeabc...", ...]
{% endcodeblock %}

This is where things start to get interesting. `tails` creates a list out of
successive tails of a list. If you were to do this on a finite list you'd get a
list of smaller and smaller elements. For example:

``` bash ghci console
ghci> tails "abcde"
["abcde","bcde","cde","de","e",""]
```

But since our input is an infinite we now have an infinite list of infinite
lists, each element starting at one letter in from the original list. Remember,
this hasn't actually been worked out yet.

{% codeblock map (take n) %}
["abcde", "bcdea", "cdeab", "deabc", "eabcd" "abcde", ...]
{% endcodeblock %}

`take n` takes the first `n` elemets of a list. Our lambda expression defines
this as the length of the list, which is 5. And `map` applies this to each
element of the list. So now we have an infinite list of 5 element lists. which is
closer to our required final output.

{% codeblock take n %}
["abcde", "bcdea", "cdeab", "deabc", "eabcd"]
{% endcodeblock %}

Applying `take n` again to the infinite list takes the first 5 elements of the
finite list

{% codeblock tail %}
["bcdea", "cdeab", "deabc", "eabcd"]
{% endcodeblock %}

Applying `tail` to the finite list takes all but the first element, and we get
the output that we were looking for. Even though along the way we had an
infinite list of infinite lists.

And yet all Haskell did was provide a promise (a _thunk_ in functional terms)
that it would provide the answer when it was asked for, and as the function
progressed the infinite list of infinite list became an infinite list of finite
lists and then a finite list. And still it didn't actually resolve all those
calls until it was actually needed. In this case, when the _ghci_ interpreter
tried to get a string to display on the screen.

Freaky or what?
