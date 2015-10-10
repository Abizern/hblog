---
title: Easy JSON formatting
tags: mac, programming, automator
description: Set up an Automator service for easily pretty formatting JSON
---

Sometimes we want to format some JSON. Here's an easy way to set up a Service in
Automator to make this easier.

Let's create a service. Open Automator and choose to create a new Service

![](http://images.abizern.org.s3.amazonaws.com/2015/10/CreateNewService.png)

From the Library, select the "Run Shell Script" action and drag it to the right.

![](http://images.abizern.org.s3.amazonaws.com/2015/10/RunShellScript.png)

In the action box type:

```
json_pp | pbcopy
```

![](http://images.abizern.org.s3.amazonaws.com/2015/10/JSONPrettyPrint.png)

This is what actually does the formatting. `json_pp` comes with OS X. This
runs the selected text through the tool and puts the result in the
pasteboard.You can paste your formatted output wherever you like.

Save the action with a name - I've called mine _JSON Pretty Print_. You should
see the new service installed under `~/Library/Services`

Rather than have to paste the result, we can define another service that
replaces the selected text with the result. Create another service as before
except that the action doesn't put the results on the pasteboard.

```
json_pp
```

![](http://images.abizern.org.s3.amazonaws.com/2015/10/JSONPrettyPrintInPlace.png)

Note that the "Output replaces selected text" option is checked.

Save this action with a name - I've called mine _JSON Pretty Print In Place_.

An advantage of using the service is that it is intelligent about when to make
the service available. If there is no selection, then the Service is not listed:

![](http://images.abizern.org.s3.amazonaws.com/2015/10/NoSelection.png)

If the selection is in a place where the text cannot be pasted, then only the
service that copies the result is presented.

![](http://images.abizern.org.s3.amazonaws.com/2015/10/Selection.png)

But if the text can be pasted in place, the Service is shown.

![](http://images.abizern.org.s3.amazonaws.com/2015/10/SelectionInPlace.png)

You can see a gif of this in action at this
[link](https://dl.dropboxusercontent.com/s/7hb3tyqexojaaxf/2E4B3531-09A5-4514-B556-556E22FBB455-12370-0000275E07F45E21.gif?dl=0) -
I'm linking to this so there isn't a constantly repeating animation while you
read this.

Of course, if you are using Emacs, you don't want to be grabbing the mouse to
get to the action, here's a lisp function I use to format JSON in place:

```commonlisp
(defun json-format ()
  "Reformats the JSON in the region for humans."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

```

**Edit** Thanks to [@chrisridd](https://twitter.com/chrisridd) for pointing out
  that I could use `json_pp`.
