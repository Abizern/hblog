---
title: "Setting up for Lisp on OS X"
description: "This is how I roll, you might choose a different path."
date: "2013-03-31"
tags: ["macos", "emacs", "lisp"]
aliases: [ "/2013/03/31/setting-up-for-lisp-on-os-x/" ]
---

This is how I roll, you might choose a different path.

1. Install Emacs. Get a nice, modern pre-built binary from
   [Emacs For Mac OS X](http://emacsformacosx.com/). It even supports fullscreen
   mode

2. Install [Emacs Prelude](http://batsov.com/prelude/) which provides a useful,
   opinionated emacs environment which is easily configured.

3. Install clisp. I use homebrew so `brew install clisp` takes care of that for
   me.

4. Install [quicklisp](http://quicklisp.org); from Terminal go to a temporary
   directory and download quicklisp.lisp with `curl -O
   http://beta.quicklisp.org/quicklisp.lisp`. Then run the lisp file from clisp
   with `clisp -i quicklisp.lisp`. As the instructions say, evaluate
   `(quicklisp-quickstart:install)`, and then evaluate `(ql:add-to-init-file)`
   to load quicklisp along with lisp.

5. Install Slime. While you have the clisp repl open, evaluate `(ql:quickload
   "quicklisp-slime-helper")`. After this runs, you will see some
   recommendations for what to put into your .emacs file. You don't need to do
   this. I did say that Prelude was opinionated, and it's done this for
   you. However, it's not set up for clisp, so instead, add this to your
   `personal/personal.el` file: `(setq slime-default-lisp 'clisp)`

6. And that should be it. You don't need to have the clisp repl running in the
   terminal anymore, and you can delete the `quicklisp.lisp` file that you
   downladed, as it's done it's job of installation now.

7. Test the setup from Emacs by running `M-x slime-mode` and you'll be taken to
   the lisp REPL.

Now that's done you can go on to the hard part of actually learning and using lisp.
