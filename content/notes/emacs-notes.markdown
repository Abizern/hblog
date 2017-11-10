---
title: "Emacs Notes"
description: "Keyboard shortcuts and usage notes."
tags: [ "emacs"]
aliases: [ "/emacs-notes/" ]
type: "note"
---

## Installation


### Emacs
```bash
brew install emacs --cocoa --srgb
```

### .emacs.d

These work for my [dotfiles](https://github.com/Abizern/prelude), based off
[Emacs Prelude](http://batsov.com/prelude/), which should be cloned to
`~/.emacs.d`. There are no submodules in this repository.

Install with the customised script

```bash
curl -L https://github.com/Abizern/prelude/raw/master/utils/installer.sh | sh
```

## General

--------------   --------------------------------------
`C-_, C-x u`     Undo, Cancel last command
`C-x, C-f`       Visit file
`C-x C-i`        Insert contents of file in buffer
`C-x C-s`        Save buffer
`C-x C-w`        Save buffer as...
`<revert-file>`  Revert to last saved version
`C-x p`          Visit buffer under point
`C-f, C-j`       Turn off ido-mode in minibuffer
`M-;`            Comment active region (`comment-dwim`)
`M-/`            Hippie expansion
`C-M-/`          Completion on current word
`M-q`            Format text to paragraph
`C-x \`          (`align-regexp`)
`C-c C-w`        Backward kill word
`S-return`       Smart open line below
`C-S-return`     Smart open line above
`C-g`            Quit command in the minibuffer
`C-u`            Prefix argument
`M-j`            Join the next line to this one
`S-r`            Recent files (ido)
-------------------------------------------------------

## Movement

-----------     --------------------------------------------
`M-<, M->`      Start / End of buffer
`M-v, C-v`      Top / Bottom of screen
`C-p, C-n`      Prev / Next Line
`C-S-p, C-S-n`  Prev / Next line (by 5)
`M-a, M-z`      Beg / End of sentence
`M-{, M-}`      Beg / End of paragraph
`C-a, C-e`      Beg / End of line
`M-b, M-f`      Backward / Forward word
`C-b, C-f`      Backward / Forward character
`C-S-b, C-S-f`  Backward / Forward Character (by 5)
`C-x C-t`       Transpose lines
`C-M v`         Scroll other window
`C-l`           Current line to middle of screen
`C-u 0 C-l`     Current line to top of screen
`C-s up`        Move current line or region up
`C-s down`      Move current line or region down
`M-g g`         Go to line
`C-=`           expand-region
---------------------------------------------------------

## Buffers

---------  ---------------------------------------
`C-x b`    ido-switch buffers
`C-x C-b`  Buffer list (IBuffer)
`C-x k`    Kill current buffer
`D, U, x`  Mark, Unmark, Apply changes
`C-y`      Bury buffer (least likely to find)
`C-x p`    Finds file under point
--------------------------------------------------

## Windows

--------   -------------------------------------
`C-c s`    Swap windows
`C-x o`    Switch to other window (cycles)
`C-x 0`    Close current window
`C-x 1`    Close other window
`C-x 2`    Split pane horizontally
`C-x 3`    Split pane vertically
--------------------------------------------------

## Frames

---------  ---------------------------------------
`C-x 5 b`  Switch to buffer in new frame
`C-x 5 0`  Destroy window with point
`C-x 5 1`  Destroy all but current window
--------------------------------------------------

## Key-Chords

-----  -------------------------------------------------------------------------
`jj`   Jump to the beginning of a word (`ace-jump-word-mode`)
`jk`   Jump to a character (`ace-jump-char-mode`)
`jl`   Jump to the beginning of a line (`ace-jump-line-mode`)
`JJ`   Jump back to previous buffer (`prelude-switch-to-previous-buffer`)
`uu`   View edits as a tree (`undo-tree-visualize`)
`xx`   Execute extended command (`execute-extended-command`)
`yy`   Browse the kill ring (`browse-kill-ring`)
`kk`   (`just-one-space`)
`KK`   (`delete-horizontal-space`)
--------------------------------------------------------------------------------


## Text Processing

-------------------     ------------------------------------
`C-s, C-r`              Search Forwards, Backwards
`RET`                   Exit Search Mode
`C-g`                   Exit search, returt point to origin
`M-%`                   Replace
(`replace-string`)      Replace all
`C-Space`               Set / Unset a mark
`C-x C-x`               Swap point and mark
(`transient-mark-mode`) Toggle transient mark mode
`C-d`                   Delete char
`M-d`                   Forward kill word
`M-del`                 Backward kill word
`C-w, M-w`              Kill, Copy region
`C-k`                   Kill line
`C-y / M-y`             Yank / Cycle kill ring
`C-m k`                 Kill word before line
`C-u C-spac`e           Swap mark in kill ring
`C-x n, C-x w`          Narrow, Widen the region
------------------------------------------------------------

## Shell

-------------    -------------------------------------------
`C-x m`          Start shell (at least with my emacs.d)
`M-x shell`      Start shell
`C-u M-x shell`  Will prompt for the name of the new shell
------------------------------------------------------------

## Registers

- Registers are named storage area.
- 62 registers a-z A-Z 0-9
- Position register - a place in a buffer
- Text register - stores text
- number register - stores numbers, most useful with macros
- View register - stores a workspace of some description. (Poor man's session,
  does not store the buffers.

-------------------- ---------------------------------------
`C-x r`              Base key chord
`M-x view-register`  Shows what's in the register
`C-x r space <name>` Stores a position in name
`C-x r g <name>`     Restores a position
`C-x r s <name>`     Stores text (region) in name
`C-x r i <name>`     Pulls out text from the regiser
                     (`i` is for insert)
`C-x r w <name>`     Stores a window config (`w` for window)
`C-x r j <name>`     Restores config
------------------------------------------------------------

## Bookmarks

Bookmarks are not saved by default, saved on quit.

(M-x bookmark-save)

----------------  --------------------------------
`C-x r m <name>`  Store a bookmark
`C-x r l <name>`  List the bookmarks file
`C-x r b <name>`  Jump to bookmark
--------------------------------------------------

## Special Characters

---------------  ---------------------------------
C-x 8 C-h        List of special characters
C-x 8 L          £
---------------  ---------------------------------

## Help

---------  ---------------------------------------
`C-h t`    Tutorial
`C-h r`    Manual
`C-h v`    Scroll other window
`C-h i`    Info buffer
`C-h v`    Describe variable
`C-h k`    Describe Key
`C-h f`    Describe Function
`apropos`  Search for concept
`<man>`    Invoke man page viewer
`C-h m`    Help for mode
--------------------------------------------------

## Projectile

----------  --------------------------------------------------------------------
`C-c p f`   Display a list of all files in the project.
            With a prefix argument it will clear the cache first.
`C-c p d`   Display a list of all directories in the project.
            With a prefix argument it will clear the cache first.
`C-c p T`   Display a list of all test files(specs, features, etc) in the project.
`C-c p g`   Run grep on the files in the project.
`C-c p b`   Display a list of all project buffers currently open.
`C-c p o`   Runs `multi-occur` on all project buffers currently open.
`C-c p r`   Runs interactive query-replace on all files in the projects.
`C-c p i`   Invalidates the project cache (if existing).
`C-c p R`   Regenerates the projects `TAGS` file.
`C-c p k`   Kills all project buffers.
`C-c p D`   Opens the root of the project in `dired`.
`C-c p e`   Shows a list of recently visited project files.
`C-c p a`   Runs `ack` on the project. Requires the presence of `ack-and-a-half`.
`C-c p c`   Runs a standard compilation command for your type of project.
`C-c p p`   Runs a standard test command for your type of project.
`C-c p z`   Adds the currently visited to the cache.
`C-c p s`   Display a list of known projects you can switch to.
`S-f`       Find file in project
`S-d`       Find directory in project
`S-g`       Run grep on project
`S-p`       Switch projects
`C-c p C-h` Help on Projectile keybindings
--------------------------------------------------------------------------------

## VC Mode

----------  --------------------------------------
`C-x v`     Diff against head
`C-x v u`   Discard changes
`C-x v l`   View Commit Log
`C-x #`     Return (after writing a commit message)
--------------------------------------------------

## CUA Mode (rectangles)

------------  ------------------------------------
(`cua-made`)  Invoke mode
`C-RET`       Enters and starts a selection
`RET`         Jumps around corners
`M-n`         Add an auto-incrementing number
--------------------------------------------------

## Magit

--------   ---------------------------------------
`C-c g`    Magit Status
`s, u`     Stage, Unstage file or hunk
`S, U`     Stage, Unstage all
`C-c C-c`  Commit after message
`C-c C-A`  Commit Amend
`P, L`     Pull, Pull
`$`        See output
`b, B`     Switch, New Branch
--------------------------------------------------

## Gist

-------------------------  -------------------------------------------
`g`                        Reload gist list from the server
`e`                        Edit current gist description
`k`                        Delete current gist
`+`                        Add file to the current gist
`-`                        Remove file from the current gist
`C-x C-s`                  Saves a buffer
`C-x C-w`                  Rename some file
`@`                        (In dired mode) Make a gist out of a file
(`gist-list`)              Gets the current gist list
(`gist-region`),           } Obvious what these do:
(`gist-buffer`),           } The Gist URL is copied
(`gist-region-or-buffer`)  } to the kill ring for convenience.
                           } Can take a `-private` suffix.
----------------------------------------------------------------------

## Haskell

### Editing

------------   -----------------------------------------------------------------
`C-c C-=`      Inserts `=`; Lines up type signatures and other pattern matches
`C-c C-pipe`   Inserts a guard
`C-c C-w`      Inserts a where
`C-c C-.`      Aligns code over a region in a sensible manner
--------------------------------------------------------------------------------

### ghc-mod

------------  ------------------------------------------------------------------
`M-c-i`       Completes name of keyword, module, class, function, types, etc
`M-/`         Complets the name of a local symbol
`M-t`         Templates:
              Begining of buffer: "module Foo where""
              Function without signature: inferred type
              Symbol without definition: foo = undefined
              Original code is replaced with hlint's suggestion if possible
`M-c-d`       Browse local document of the module on the current line
`C-u M-c-d`   Browse Hackage document
`M-c-m`       Loads information of symbols for modules in current buffer.
              Type this when adding a new line to import a module.
              Executed by the idle timer anyway.
`C-x C-s`     Saves buffer and runs a syntax check.
`C-c C-c`     Toggle GHC and HLint for syntax. GHT is used initially
`M-n`         Next warning or error
`M-p`         Previous warning or error
`M-?`         Displays the warning/error message in the current line
`C-c C-i`     Displays the info of this expression in another window
`C-c C-t`     Displays the type of this expression in the minibuffer.
              Type multiple times to enlarge the expression
`C-c C-e`     Displays the expanded Template Haskell
`C-c C-m`     Insert "import Module" for this function. "hoogle" command is
              required and "hoogle data" should be done beforehand
`C-c C-j`     In the beginning of the buffer: errors of other files are
              displayed. C-c C-j on the errors jumps to the first file of
              the error sources.
`C-c <`       Make the indentation of the region shallower
`C-c >`       Make the indentation of the region deeper
--------------------------------------------------------------------------------

### Interpreter

--------------------  ----------------------------------------------------------
`C-c C-l`             Load current buffers into the interpreter
`C-c C-r`             Reload current Haskell interpreter session
`C-c C-t`             Gets :type for symbol at point, and remembers it
`C-u C-c C-t`         Inserts a type annotiation, for symbol at point
                      on the line above
`C-c C-i`             Gets :info for symbol at point
`C-c M-.`             Find definition for interpreted symbol at point
`C-c C-b or C-c C-z`  Switch to interpreter - start one if needed
`C-c C-d`             Find Haddock documentation about symbol
`C-c TAB`             Query the Haskell interpreter for info of the expression
`C-c C-v`             Check current buffer's file with hlint
--------------------------------------------------------------------------------

## Ruby

### Editing

--------------  ----------------------------------
`C-M-a, C-M-e`  Beginning, End of defun
`C-M-b, C-M-f`  Back, forward sexp
`C-M-h`         Backward kill word
`C-M-n, C-M-p`  End, Beginning of block
`C-M-q`         Indent expression
`C-c C-c`       Comment region
--------------------------------------------------

### Interpreter (inf-ruby)

----------  --------------------------------------
`C-c C-s`   inf-ruby
`C-c C-z`   switch to inf
`C-c C-x`   Send block
`C-c C-r`   Send region
`C-M-x`     send definition
`C-c C-l`   Load file
`C-c M-b`   Send block and go
`C-c M-r`   Send region and go
`C-c M-x`   Send definition and go
--------------------------------------------------
