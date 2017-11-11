---
title: "Debug Logging In Swift"
description: "Print strings to the console only when in Debug mode."
date: "2015-02-01"
tags: ["Swift", "Xcode"]
aliases: [ "/2015/02/01/debug-logging-in-swift/" ]
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

```swift
import Foundation

/// Prints the filename, function name, line number and textual representation of `object` and a newline character into the standard output if the build setting for "Active Complilation Conditions" (SWIFT_ACTIVE_COMPILATION_CONDITIONS) defines `DEBUG`.
///
/// The current thread is a prefix on the output. <UI> for the main thread, <BG> for anything else.
///
/// Only the first parameter needs to be passed to this funtion.
///
/// The textual representation is obtained from the `object` using `String(reflecting:)` which works for _any_ type. To provide a custom format for the output make your object conform to `CustomDebugStringConvertible` and provide your format in the `debugDescription` parameter.
/// - Parameters:
///   - object: The object whose textual representation will be printed. If this is an expression, it is lazily evaluated.
///   - file: The name of the file, defaults to the current file without the ".swift" extension.
///   - function: The name of the function, defaults to the function within which the call is made.
///   - line: The line number, defaults to the line number within the file that the call is made.
public func loggingPrint<T>(_ object: @autoclosure () -> T, _ file: String = #file, _ function: String = #function, _ line: Int = #line) {
    #if DEBUG
        let value = object()
        let fileURL = NSURL(string: file)?.lastPathComponent ?? "Unknown file"
        let queue = Thread.isMainThread ? "UI" : "BG"
        
        print("<\(queue)> \(fileURL) \(function)[\(line)]: " + String(reflecting: value))
    #endif
}


/// Outputs a `dump` of the passed in value along with an optional label, the filename, function name, and line number to the standard output if the build setting for "Active Complilation Conditions" (SWIFT_ACTIVE_COMPILATION_CONDITIONS) defines `DEBUG`.
///
/// The current thread is a prefix on the output. <UI> for the main thread, <BG> for anything else.
///
/// Only the first parameter needs to be passed in to this function. If a label is required to describe what is being dumped, the `label` parameter can be used. If `nil` (the default), no label is output.
/// - Parameters:
///   - object: The object to be `dump`ed. If it is obtained by evaluating an expression, this is lazily evaluated.
///   - label: An optional label that may be used to describe what is being dumped.
///   - file: he name of the file, defaults to the current file without the ".swift" extension.
///   - function: The name of the function, defaults to the function within which the call is made.
///   - line: The line number, defaults to the line number within the file that the call is made.
public func loggingDump<T>(_ object: @autoclosure () -> T, label: String? = nil, _ file: String = #file, _ function: String = #function, _ line: Int = #line) {
    #if DEBUG
        let value = object()
        let fileURL = NSURL(string: file)?.lastPathComponent ?? "Unknown file"
        let queue = Thread.isMainThread ? "UI" : "BG"
        
        print("--------")
        print("<\(queue)> \(fileURL) \(function):[\(line)] ")
        label.flatMap{ print($0) }
        dump(value)
        print("--------")
    #endif
}
```

An updated version of this is available on [github](https://github.com/JungleCandy/LoggingPrint/)

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

**Update Oct 8, 2015**

Changed to support Swift 2.

Thanks to [rob_rix](https://twitter.com/rob_rix) and
[jl_hfl](https://twitter.com/jl_hfl) for the suggestion.




