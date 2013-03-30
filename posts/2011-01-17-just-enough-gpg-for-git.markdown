---
title: Just Enough GPG for git
tags: osx, web, git
---

It came about that I wanted to do some work with git and signed tags. It's been
a while since I had looked at this, I've got some old entries up on keyservers
that date back to 1999, and never on a Mac.

It turns out that it is quite simple to set up a minimal
[GPG](http://en.wikipedia.org/wiki/Pretty_Good_Privacy) environment – one that
lets you work on the command line without having to set it up for Mail.app. This
is about all I need it for.

<!-- more  -->

The [GPGTools project](http://www.gpgtools.org/index.html) has recently
resurrected the [MacGPG project](http://macgpg.sourceforge.net/) to provide
email encryption and tools to the Mac. It is still in development, and I didn't
want to mess about with my Mail installation so rather than install the complete
set of tools, I chose to install [MacGPG2](http://www.gpgtools.org/macgpg2.html)
and [GPGKeychain Access](http://www.gpgtools.org/keychain.html)

[MacGPG2](http://www.gpgtools.org/macgpg2.html) is the
[OpenPGP](http://en.wikipedia.org/wiki/Pretty_Good_Privacy#OpenPGP)
implementation for the Mac. This installs gpg2 into /usr/local/bin and gpg is
symlinked to gpg2. I only mention this because although the commands can all be
issued as gpg, you get to the documentation by using man gpg2, not man
gpg. Installation is through an installer package.

[GPGKeychain Access](http://www.gpgtools.org/keychain.html) does not integrate
with the Mac Keychain as the name might suggest, but provides a window to look
at and manage the keys that you have on your system. These are usually under
~/.gnupg/ Run the installer, and create your keys. It's quite simple and there
is a video on the project page. However, there are a couple of things that you
should keep in mind. If you forget your passphrase you can't use your private
key anymore. And if you've published the key, you won't be able to revoke it and
it will just sit around on keyservers. So, set an expiry date on your keys in
case you do lose the private key or passphrase. As the expiry date comes up just
extend it again.

There is no key-server configured. There seems to be a ticket for this to be
implemented in some future milestone. Until then, create a file called gpg.conf
under ~/.gnupg and put this line in it:

    keyserver hkp://pgp.mit.edu

And that is just enough so that when you use the menu items that send and get
keys from keyservers they will work. As far as I know, these servers talk to
each other, so writing to one makes the key visible on the others.

Synchronisation of keys is an issue. If you are adventurous you could add more
entries to the gpg.conf file to use a central location for the keyrings,
somewhere like Dropbox or iDisk, so that all your machines can use the same
files. But, it's just as easy to export the keys as text and use those files to
keep different machines in sync. Partcularly if you will be using gpg rarely.

This has been a companion piece to the non-Mac centric
[365Git](http://365git.tumblr.com/) post about
[signed tags](http://365git.tumblr.com/post/2796779828/signing-a-git-tag).
