---
title: An Improved Reverse Words Script
description: There's always room to do better
tags: haskell

---

About 6 months ago I posted
[a solution](http://abizern.org/2012/04/09/reverse-words-with-haskell/) to the
Google Code Jam problem
[Reverse Words](http://code.google.com/codejam/contest/351101/dashboard#s=p1
"Original problem statement")

I've become more comfortable with Haskell since then, so here's an improved
solution.

<figure>

``` haskell
module Main where

{-
Problem statement:
http://code.google.com/codejam/contest/351101/dashboard#s=p1

Run as a script with runghc, or compile with ghc --make -O2

input and output is by redirection;

$ ./reverse_words < input_file > output_file
 -}

import Control.Monad

main :: IO ()
main = do
  n <- readLn
  forM_ [1..n] $ \i -> do
    solution <- fmap (unwords . reverse . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ solution

```

<figcaption>Improved Reverse Words With Haskell [view as gist]( https://gist.github.com/Abizern/3986006)</figcaption>
</figure>

I'm not one who believes that shorter, terser code is necessarily better, but
that isn't what makes this version an improvement. Nor even the use of Monads;
Haskell's sexy buzzword. I think this code is better because it has less noise
than the original solution.

Code Jam problems have well defined inputs and required outputs, and there is no
need to load the whole input file when the script starts. Normally, I would be
all for splitting out code into smaller, descriptively named functions, but in
this small case that's just more typing and more lines getting in the way of the
actual solution. I think that this version is more readable because there are
less lines of code that don't contribute directly to the solution.

And Code Jam has a time limit, so smaller code is faster to write and easier to
debug.
