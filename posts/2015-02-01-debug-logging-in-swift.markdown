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

So, I wrote this (available as a
[gist](https://gist.github.com/Abizern/a81f31a75e1ad98ff80d#file-loggingprintln-swift)

```swift
/**
Prints the filename, function name, line number and textual representation of `object` and a newline character into 
the standard output if the build setting for "Other Swift Flags" defines `-D DEBUG`.

Only the first parameter needs to be passed to this funtion.

The textual representation is obtained from the `object` using its protocol conformances, in the following 
order of preference: `Streamable`, `Printable`, `DebugPrintable`. Do not overload this function for your type.  
Instead, adopt one of the protocols mentioned above.

:param: object   The object whose textual representation will be printed.
:param: file     The name of the file, defaults to the current file without the ".swift" extension.
:param: function The name of the function, defaults to the function within which the call is made.
:param: line     The line number, defaults to the line number within the file that the call is made.
*/
func loggingPrintln<T>(object: T, _ file: String = __FILE__, _ function: String = __FUNCTION__, _ line: Int = __LINE__) {
    #if DEBUG
        let file = file.lastPathComponent.stringByDeletingPathExtension
    
        println("\(file).\(function)[\(line)]: \(object)")
    #endif
}

```

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





