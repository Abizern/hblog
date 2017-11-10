---
title: GCJ B - Cookie Clicker Alpha
tags: haskell, gcj
description: 
---

Another problem from This year's Google Code Jam.

The gist of the problem is to work out whether buying additional capacity for
cookie production would result is reaching the quota faster than not buying
additional capacity.

Haskell is suited to this for a couple of reasons. Firstly, its' easy to work
with infinite lists. so I can create list of the cumulative times for creating
factories and reaching the target. And to calculate the cumulative target I'm
using the `scanl1` function to turn the infinite list of factory times into an
infinite list of partial sums. I'm using `scanl1` because it starts at 0, which
is important as one of the gotcha's with this is that it may be faster to just
generate cookies rather than buying a factory in the first instance.

Secondly, pattern matching. With the infinite list I'm stopping when the time to
reach the target starts growing again. Pattern matching makes this very easy to do.

```haskell
module Main where

import Control.Monad
import Numeric


-- https://code.google.com/codejam/contest/dashboard?c=2974486#s=p1
-- Input and output with standard redirection operators

factoryTimes :: Double -> Double -> [Double]
factoryTimes c f = 0.0 : [ c / (2.0 + k * f) | k <- [0.0, 1.0 ..]]

productionTimes :: Double -> Double -> [Double]
productionTimes x f = [ x / (2.0 + k * f) | k <- [0.0, 1.0 ..]]

times :: Double -> Double -> Double -> [Double]
times c f x = zipWith (+) production factory
  where production = productionTimes x f
        factory = scanl1 (+) $ factoryTimes c f

firstMinimum :: [Double] -> Double
firstMinimum (x:y:ys) = if x < y
                        then x
                        else firstMinimum (y:ys)

solve :: Double -> Double -> Double -> Double
solve c f x = firstMinimum $ times c f x

main :: IO ()
main = do
  t <- getLine
  forM_ [1..read t :: Int] $ \i -> do
    [c, f, x] <- fmap (map read . words) getLine
    let result = solve c f x
    putStrLn $ concat ["Case #", show i, ": ", Numeric.showFFloat (Just 7) result ""]
    
```
