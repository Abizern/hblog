---
title: "Swift Function Overloading By Return Type"
description: "As Objective-C developers, we often forget that we can overload functions by return type."
date: "2015-10-11"
tags: ["Swift"]
aliases: [ "/2015/10/11/swift-function-overloading-by-return-type/" ]
---

As Objective-C developers, we often forget that we can overload functions by
return type. This can often help us create neat APIs. This is best illustrated
by an example.

I posted a
[wrapper for timer dispatch sources](http://abizern.org/2015/10/10/a-swift-repeating-timer/)
yesterday. While I was doing it I noticed a discrepancy with GCD. When creating
a timer dispatch source, times were passed as nanoseconds as a `UInt64`, but the
`dispatch_after()` function took nanoseconds as an `Int64`. Such things are what
cause hair-pulling and fighting with the type system.

But most of the time we just want to pass times around as `NSTimeInterval`s as
seconds, but we have to convert this to a `UInt64` or an `Int64`. It's a better
idea to wrap these creating methods, and function overloading lets us use the
same descriptive name for these functions with different return types. So, here
is some code which you can put in a playground that demonstrates this:

<script src="https://gist.github.com/Abizern/c030bd9674b2ad881b44.js"></script>

I'm extending `NSTimeInterval` which is a typealias for `Double` just to make it
clear that these new functions apply to times.

Note that there are two functions called `nSecs()`, but they return different
values. And the strong type system ensures the correct function is used. `dispatch_source_set_timer()` uses the  `nSecs() -> UInt64` values, and
the `dispatch_after()` uses `nSecs() -> Int64` function

This example also demonstrates that we should write functions for such
translations, which separates the concerns of creating and transforming values
from the use of those values. It's something that I don't do enough of, and I
encourage you do try and do this where you can.

