---
title: "UUID Strings with Cocoa"
description: "This used to be a thing until we got the NSUUID class with iOS 6 and macOS 10.8"
date: "2012-10-25"
tags: ["macOS", "iOS", "UUID", "Objective-C"]
aliases: [ "/2012/10/25/uuid-strings-with-cocoa/" ]
---

This used to be a thing until the iOS 6 and OS X 10.8 compatible
[NSUUID Class](http://developer.apple.com/library/mac/#documentation/Foundation/Reference/NSUUID_Class/Reference/Reference.html)
became available.

## New

This is how you can do it now:

```
NSString *uuidString = [[NSUUID UUID] UUIDString];
// Generates: 7E60066C-C7F3-438A-95B1-DDE8634E1072
```

## Old

Here's a method you can put in a class, with the correct ARC casts on ownership, that
returns a UUID. It's a fairly common technique, and you'll even see versions of
it where people have created a category on NSString for this.

```objc
- (NSString *)uuidString {
    // Returns a UUID

    CFUUIDRef uuid = CFUUIDCreate(kCFAllocatorDefault);
    NSString *uuidStr = (__bridge_transfer NSString *)CFUUIDCreateString(kCFAllocatorDefault, uuid);
    CFRelease(uuid);

    return uuidStr;
}
```

And to use it:

```
NSString *uuidString = [self uuidString];
// Generates D5CB0560-206F-4581-AA25-1D6A873F3526
```

## NSProcessInfo

A common use for unique strings is to name files and directories
within a program so that they do not clash. Since iOS 2 and OS X 10.0 there has
been the `globallyUniqueString` method in
[NSProcessInfo](https://developer.apple.com/library/mac/#documentation/Cocoa/Reference/Foundation/Classes/NSProcessInfo_Class/Reference/Reference.html)
which returns a string that is unique for the network and process. So, for a
_good enough_ unique string this is probably a better method to use:

```
NSString *uuidStr = [[NSProcessInfo processInfo] globallyUniqueString];
// generates 56341C6E-35A7-4C97-9C5E-7AC79673EAB2-539-000001F95B327819
```
