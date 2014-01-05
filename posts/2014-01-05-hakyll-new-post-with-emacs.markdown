---
title: Hakyll New Post With Emacs
tags: meta, hakyll, emacs
description: Create a new hakyll page from within Emacs.
---

When this blog was created using Octopress, a new post could be created by
simply running `rake new_post` in the Terminal. There is no such convenience in
Hakyll, which is currently used as the generator. A small thing, but I wanted to
fix it.

I could have created a similar Rake task, or shell script to do this, and there
are even examples to be [found](http://jaspervdj.be/hakyll/examples.html) on
Hakyll's site. But since I'm usually in Emacs when I want to write a new post, I
thought it would be a good excuse to write a little lisp. And so, here is my
first attempt.

```commonlisp
(defun hakyll-site-location ()
  "Return the location of the Hakyll files."
  "~/Sites/hblog/")

(defun hakyll-new-post (title tags)
  "Create a new Hakyll post for today with TITLE and TAGS."
  (interactive "sTitle: \nsTags: ")
  (let ((file-name (hakyll-post-title title)))
    (set-buffer (get-buffer-create file-name))
    (markdown-mode)
    (insert
     (format "---\ntitle: %s\ntags: %s\ndescription: \n---\n\n" title tags))
    (write-file
     (expand-file-name file-name (concat (hakyll-site-location) "posts")))
    (switch-to-buffer file-name)))

(defun hakyll-new-note (title)
  "Create a new Note with TITLE."
  (interactive "sTitle: ")
  (let ((file-name (hakyll-note-title title)))
    (set-buffer (get-buffer-create file-name))
    (markdown-mode)
    (insert (format "---\ntitle: %s\ndescription: \n---\n\n" title))
    (write-file
     (expand-file-name file-name (concat (hakyll-site-location) "notes")))
    (switch-to-buffer file-name)))

(defun hakyll-post-title (title)
  "Return a file name based on TITLE for the post."
  (concat
   (format-time-string "%Y-%m-%d")
   "-"
   (replace-regexp-in-string " " "-" (downcase title))
   ".markdown"))

(defun hakyll-note-title (title)
  "Return a file name based on TITLE for the note."
  (concat
   (replace-regexp-in-string " " "-" (downcase title))
   ".markdown"))
```

I'm not much of a lisper, and it probably took me longer to write than the time
it will save me, but that doesn't matter. Firstly; it might be useful to
somebody else, and so the cumulative time saved could be greater.

Secondly, my day job means I spend most of my coding time in Xcode, which can't
be customised as Emacs can be, and if I don't spend time writing and learning
lisp, I'll have no chance of getting better at it.

So, I think it's not bad as a first attempt, although it could obviously be
refactored, and you can follow the history of it in my dotfiles
[repository](https://github.com/Abizern/prelude/blob/master/personal/hakyll.el)
if you want to see how it could be developed (or even, you know, help me out
with it).
