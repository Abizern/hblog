---
title: "A Swift Repeating Timer"
description: "A Swift function to create and start a timer dispatch source."
date: "2015-10-10"
tags: ["swift"]
aliases: [ "/2015/10/10/a-swift-repeating-timer/" ]

---

We frequently (excuse the pun) need to schedule a repeated action.

The way to do this was usually to use `NSTimer`'s
`scheduledTimerWithTimeInterval(_:,target:,selector:,userInfo:,repeats:)`, which
needed a callback, and had the hidden pitfall of the target being strongly
referenced by the runloop that this timer was scheduled on. There are extensions
to NSTimer that allow the used of blocks instead (I even wrote one myself), but
there is another way.

Grand Central Dispatch provides
[dispatch sources](https://developer.apple.com/library/mac/documentation/General/Conceptual/ConcurrencyProgrammingGuide/GCDWorkQueues/GCDWorkQueues.html)
for efficient interaction with the underlying system. One such source type is
`DISPATCH_SOURCE_TYPE_TIMER`. So here as a Swift function that creates and
starts such a timer.

<script src="https://gist.github.com/Abizern/cf26af397ebe66284002.js"></script>

Since I'm passing in `NSTimeInterval`s this is probably best used for short
lived timers, but it extracts a lot of the C boilerplate that is needed to
create the dispatch source.



