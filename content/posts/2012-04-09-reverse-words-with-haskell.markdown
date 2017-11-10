---
title: "Reverse Words With Haskell"
description: "A practice problem from Google Code Jam."
date: "2012-04-09"
tags: ["haskell", "gcj"]
aliases: [ "/2012/04/09/reverse-words-with-haskell/" ]
---

As part of my preparation for [Google Code Jam](http://code.google.com/codejam)
I've been working through the practice problems with Haskell.

I made a screencast on solving the
[Reverse Words](http://code.google.com/codejam/contest/351101/dashboard#s=p1)
problem, along with the mechanics of getting the problem inputs and submitting solutions. The style was heavily influenced by a sequence on
[Data Driven Programming in Haskell](http://youtu.be/045422s6xik?hd=1) by
[Jonas Tullus](http://entirelysubjective.com/programming/data-driven-programming-haskell-1/)

I moved at a slower pace, and talking and typing seem to have confused my brain
so I think I lapsed into gibberish in a couple of places. (If you know me,
you'll know that's nothing new).

<iframe width="560" height="315" src="//www.youtube.com/embed/_tgv3HVgOMc" frameborder="0" allowfullscreen></iframe>

Here's the cleaned up source file.

``` haskell
module Main where

{-
 - Problem Statement:
 - http://code.google.com/codejam/contest/351101/dashboard#s=p1
 -
 - Usage either compile or use runhaskell / runghc
 - Pass the input file as the sole command line argument
 - Redirect output if you want the results to go in a file
 -}

import IO
import System.Environment
import Data.List

reverseWords :: String -> String
reverseWords = unwords . reverse . words

boilerPlate :: [String]
boilerPlate = ["Case #" ++ show n ++ ": " | n <- [1..]]

standardOutput :: [String] -> [String]
standardOutput = zipWith (++) boilerPlate

main =  do
  (f:_) <- getArgs
  file  <- readFile f

  let cases     = tail $ lines file
      solutions = standardOutput $ map reverseWords cases
  putStrLn $ unlines $ solutions

```

I need to learn more Emacs shortcuts :(
