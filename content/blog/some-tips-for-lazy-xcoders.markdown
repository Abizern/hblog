---
title: "Some Tips for Lazy Xcoders"
description: "Handy Xcode tips."
date: "2009-11-11"
tags: [ "Xcode" ]
aliases: [ '/2009/11/11/some-tips-for-lazy-xcoders/' ]
---

We all know what we should be doing when writing code. Each methodology you
choose to use has it's own best practices, whether it's working from full
specifications, writing unit tests first, programming in pairs, yadda,
yadda. But, as developers, we're only human, and we're lazy. We have tools to
make things easy for us. Here are a few tips that you can use to help when
you're not as rigorous in your coding as you should be.

## Use the static analyser.

You can use the
[Clang Static Analyser](http://arstechnica.com/apple/reviews/2009/08/mac-os-x-10-6.ars/9#compilers)
in Xcode by setting a build option. This will find a whole host of errors in
your code, even down to unconventionally named functions.

![ClangBuildSetting.png](http://images.abizern.org.s3.amazonaws.com/2009/11/ClangBuildSetting.png)

Now you can just code away and have the compiler pick up your mistakes when you
run 'Build and Analyze' (⌘+Shift+A).

## Find your mistakes quickly.

Any real application you develop will have a large number of resources that need
to be copied to your application bundle. The default projects that Xcode create
for you will copy these files first before compiling.

![XcodeDefault.png](http://images.abizern.org.s3.amazonaws.com/2009/11/XcodeDefault.png)


But, the lazy Xcoder knows that there are probably errors in the code that need
to get flagged by the compiler, so this copying is a waste of time. Reorder the
build steps by dragging so that the compilation is done first.

![XcodeRecommended.png](http://images.abizern.org.s3.amazonaws.com/2009/11/XcodeRecommended.png)

Your builds will now break early (and often!).

## Don't fear the version controller.

I'm going to stick my neck out and say that if you're not using version control
you're an idiot. The lazy Xcoder uses a powerful system that lets him or her
branch easily, make lots of little changes, and lots of mistakes (that can be
backed out). These changes can then be bundled into larger commits to be merged
into the main branch so that your co-workers don't see what an idiot you've
been.

One such version control system is [git](http://git-scm.com/). The lazy Xcoder
writes a bit of code, checks it in out of habit and then compiles. The compiler
picks up the mistakes, and he or she fixes them. Rather than make a new commit,
and retype the commit message, just call:

``` bash
git commit --amend -a -C HEAD
```

This will bring up the previous commit message in the editor, which you can
amend if you wish. This new commit will replace the previous one. The `-a`
option means you don't even need to do a git add and the `-C HEAD` option means
it will use the commit message from the last commit.

Of course, if you're a rockstar programmer, you don't make mistakes at this
level. But I'm not, and I prefer to work with human nature rather than against
it.
