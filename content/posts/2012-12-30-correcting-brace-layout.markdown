---
title: "Correcting Brace Layout"
description: "I wrote a Ruby Gem."
date: "2012-12-30"
tags: ["ruby", "objc", "xcode"]
aliases: [ "/2012/12/30/correcting-brace-layout/" ]
---

I wrote a [small rubygem](http://abizern.org/fixbraces/) called _fixbraces_ to
move the opening brace of a conditional to the same line as the opening
statement.

So now I can correct all the Xcode generated stubs that look like:

``` objectivec
- (void)someMethod
{
    // some code here
}
```

Into my preferred format:

``` objectivec
- (void)someMethod {
    // some code here
}
```

Which fits with my personal coding standards.
