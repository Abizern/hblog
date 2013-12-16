---
title: Git Ignore file
description: My recommended .gitignore
tags: git
---

A while ago I answered a couple of questions on Stack Overflow using my
`.gitignore` file as an example. I find it strangely satisfying to find that
there are projects on GitHub that use it, and even the odd blogs has put it up
as well.

I might as well have it on my own site:

```
# Mac OS X
*.DS_Store

# Xcode
*.pbxuser
*.mode1v3
*.mode2v3
*.perspectivev3
*.xcuserstate
project.xcworkspace/
xcuserdata/

# Generated files
*.o
*.pyc
*.hi

#Python modules
MANIFEST
dist/
build/

# Backup files
*~.nib
\#*#
.#*
```

It's particularly suited for Xcode. It excludes the workspace settings, which
also includes the breakpoints, so don't be surprised if they don't carry over in
clones. It also excludes the build schemes unless you mark them as shared.

I've got this as a global ignore file for my system - and if I'm going to push a
repository elsewhere I just copy it into the repo's directory.

It's simple to do. If you're not familiar with ignore files I've written about
the
[Three ways of excluding files with Git](http://365git.tumblr.com/post/519016351/three-ways-of-excluding-files)
elsewhere.
