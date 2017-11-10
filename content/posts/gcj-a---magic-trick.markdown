---
title: "GCJ A - Magic Trick"
description: "Solution to to Google Code Jam Magic Trick."
date: "2014-04-18"
tags: ["Haskell", "Google-Code-Jam"]
aliases: [ "/2014/04/18/gcj-a---magic-trick/" ]
---

I got through the qualification round for Google Code Jam 2014. I usually manage
to get through this stage; it's the first round that I haven't managed to get
past yet.

The easiest question was A-Magic Trick. Nothing much to do here but count the
number of similarities between two arrays, easily achieved with the `interact`
method.

The only gotcha in this problem was that the index of the rows to check is
1-based, and most programming languages use 0-based indexing.

```haskell
module Main where

import Control.Monad
import Data.List

-- https://code.google.com/codejam/contest/dashboard?c=2974486#s=p0
-- Input and output with standard redirection operators

solve :: [Int] -> [Int] -> String
solve a b
  | l == 1 = show $ head common
  | l > 1  = "Bad magician!"
  | otherwise = "Volunteer cheated!"
  where common = a `intersect` b
        l = length common

main :: IO ()
main = do
  t <- getLine
  forM_ [1..read t :: Int] $ \i -> do
    choice1 <- fmap read getLine
    initial <- replicateM 4 $ fmap (map read . words) getLine
    choice2 <- fmap read getLine
    final <- replicateM 4 $ fmap (map read . words) getLine
    let r1 = initial !! (choice1 - 1)
        r2 = final !! (choice2 - 1)
    putStrLn $ concat ["Case #", show i, ": ", solve r1 r2]
```
