---
title: "Missing Xcode Toolbar"
description: "How to restore it."
date: "2013-01-29"
tags: ["ios", "macos", "xcode"]
aliases: [ "/2013/01/29/missing-xcode-toolbar/" ]
---

I'm sure you've been there.

You're working in Xcode, and you have a file opened on another monitor in
another window. Unfortunately, you forget about it, and when you close your
project and reopen it again, the toolbar is missing.

![Missing Toolbar](http://images.abizern.org/2013/01/Missing%20Xcode%20Toolbar.png)

It can be frustrating, because Xcode remembers this, and every time you open the
project it will open without a toolbar. You can put it back by right-clicking in
what is left of the toolbar and choosing one of the options.

![Toolbar Menu](http://images.abizern.org/2013/01/Xcode%20Toolbar%20Menu.png)

But the gotcha that has bit me twice now, and prompted me to write this to
remind myself of it, is that you need to choose the "Show Toolbar" menu item
rather than one of the other two preferences. I was incorrectly choosing my
preference of "Icon Only" and wondering why my preference wasn't sticking.
