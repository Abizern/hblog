---
title: "Integrating Git with SubEthaEdit and Changes App"
description: "Adding an external diff app to SubEthaEdit."
date: "2009-11-09"
tags: ["SubEthaEdit", "Git"]
aliases: [ "/2009/11/08/integrating-git-with-subethaedit-and-changes/" ]
---

A while ago, I read a
[nice write up](http://schinckel.net/2008/04/26/mercurial-with-os-x-gui-tools/)
about using [mercurial](http://mercurial.selenic.com/) with
[SubEthaEdit](http://www.codingmonkeys.de/subethaedit/) and
[Changes](http://connectedflow.com/changes/). Here's how to do the same thing
with [git](http://git-scm.com/) instead of
[mercurial](http://mercurial.selenic.com/), separated into two parts in case you
just want to apply one set of changes.

## Changes Support

**Step One: Make sure chdiff is installed.**

Open Changes.app and from the Changes menu select "Install terminal
utility". This will install the chdiff utility which is used by the script.

**Step Two: Create a shell script to send diffs to Changes**

Create a shell script with the following contents.

```
#!/bin/sh
[ $# -eq 7 ] && /usr/bin/env chdiff "$2" "$5"
```

Where you save this and what you call it is up to you. Mine is called
'.gitchanges', saved it at the root of my home directory. Make sure the script
is executable.

**Step Three: Edit the .gitconfig file to use this script to handle diffs.**

Open your `~/.gitconfig` file. This should already exist, at the very least it
will contain you name and email. under the section called `[diff]` add the
location and name of the file.  You may have to edit this to make sure it points
to the name and location you chose in Step Two. (Make sure to use the correct
path in your setup)

```
[diff]
external = <path to file>/.gitchanges
```

## SubEthaEdit Support

**Step One: Download and install the mode file for SubEthaEdit**

I wrote a SubEthaEdit mode for this which you can download from the
[github project page](http://abizern.github.com/gitcommit.mode/). Please feel
free to fork it and send me patches.

**Step Two: Edit the .gitconfig file to use SebEthaEdit as an external editor.**

Open your `~/.gitconfig` file. This time, under the `[core]` section, add the
following line:

```
[core]
editor = /usr/bin/see -w -r -o new-window -j 'git editor' -m gitCommit -g 1:0
```

All those flags may seem daunting, but they are quite self-explanatory: the `-w`
flag makes the Terminal wait for a response from SubEthaEdit. `-r` brings
Terminal to the front after you're done editing. `-o new-window` opens a new
window for editing. I prefer this to having a new tab appear in whatever window
I was working on. `-j 'git editor`' this sets the text that appears in the title
bar, which you can change as you wish. `-m gitCommit` is what sets the mode to
be used for editing. and `-g 1:0` puts the caret at the beginning of the file.

Now, when git asks you to write a commit message, or pick commits when running
`git rebase -i` a SubEthaEdit window will open as the commit message
editor. Make whatever changes you need then save the file (⌘-S) and then close
the window (⌘-W) for these changes to take effect.
