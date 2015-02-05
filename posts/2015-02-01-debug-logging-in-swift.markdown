---
title: Debug Logging In Swift
tags: swift
description: Print strings to the console only when in Debug mode.
---

In the last [iOSDevWeekly](https://iosdevweekly.com/issues/183) Dave Verwer
listed Art Sabintsev's
[PrintLnMagic](https://github.com/ArtSabintsev/PrintlnMagic) - a small function
that reproduces the common DLog pattern that many use which not only prints a
value, but the filename, the function name and the line of the call. Which is
really handy in debugging.

I use a version of [Dlog](https://gist.github.com/Abizern/325926) myself, which
has the added benefit of only outputting to the console when in Debug
configurations, which PrintLnMagic does not do.

So, I wrote this

<script src="https://gist.github.com/Abizern/a81f31a75e1ad98ff80d.js"></script>

I'm not overriding `printLn()` because I'm not sure clobbering such a widely
used system function is a good idea.

Of course Swift projects don't work the same way as Objective-C projects, so it
isn't enough to just have the debug configuration, this needs to have the `-D
DEBUG` flag set in the "Other Swift Flags" section under Debug.

![](http://images.abizern.org.s3.amazonaws.com/2015/01/Debug%20flag%20in%20swift.png)

Installation is simple enough, this is just a single bare function, so just
download the file and add it to your project. Call `loggingPrintln()`
just as you would `println()`, only passing a value for the first parameter; the
defaults will take care of the rest.

**Update Feb 5, 2015**

Rather than just passing an object or a value, the function can now take an
expression for the first parameter. That way, the expression is only evaluated
if the function body runs. Laziness is a virtue.

Thanks to [@rob_rix](https://twitter.com/rob_rix) and
[@jl_hfl](https://twitter.com/jl_hfl) for the suggestion.




