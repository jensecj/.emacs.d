# The look
![Screenshot of my emacs in action](emacs-screen.png?raw=true "Screenshot of my emacs in action")
# My emacs

This was originally a fork of [magnars' great emacs settings](https://github.com/magnars/.emacs.d)
but has since then evolved into a beast better fitting my own preferences.

I would recommend starting out with a blank emacs and then dig through this repo 
for useful nuggets, instead of forking it directly.

It should however be possible to simply clone the entire repo, drop it into a clean 
install of emacs, and it should set everything up properly.

The key bindings are optimized for a [danish keyboard layout](http://fontmeme.com/images/danish-keyboard-550x183.png).

You can check out all the package settings and key bindings if you look through
setup-packages.el and key-bindings.el.

 * You quit emacs with `C-x r q`, mnemonic *Really Quit*.

### Custom key bindings

* `M-j`         Joins the region or the line below the current line
* `C-c C-k`     Evaluate current buffer
* `C-c k`       Evaluate current region
* `C-c n`       Indent, remove trailing white space and untabify the entire buffer
* `<escape>`    enable god mode


### Ace-jump-mode

* `C-ø` 		Quickly jump to the start of any word in the buffer
* `C-Ø` 		Quickly jump to any character inside a word
* `C-'` 		Quickly jump to any line

### Multiple-cursors

* `C-d` 		To put a new cursor at the next thing like the current region
* `C-S-d` 		To put a new cursor at all things like the current region
* `C-M-a`		Drops a rectangular anchor, then use up and down to move

### Expand-region

* `M-e`			Expand region onto the next region
* `C-M-e`		Contracts back to the previous region

### Move-text

* `C-S-<up, down>` 	Move a line / region up or down

### Change-inner

* `M-i` 	    Copy contents of inner region
* `M-o` 	    Copy contents of outer region
* `M-I` 	    Change contents of inner region
* `M-O` 	    Change contents of outer region

### Files

* `C-x C-f` 	Open a file. Starts in the current directory
* `C-x f` 		Open a recently visited file
* `C-x C-s` 	Save this file
* `C-x C-w` 	Save as ...
* `C-x t` 	    Force save this file
* `C-x C-j` 	Jump to this files' current directory
* `C-x b` 	    List all open files (buffers)
* `C-x C-b`     Jump between all open files (buffers)
* `C-x C-r` 	Reopen file using sudo

### Cut copy and paste

* `C-space` 	Start marking stuff. C-g to cancel.
* `C-w` 		Cut (aka kill) current line or region
* `C-k` 		Cut till end of line
* `C-S-k` 		Cut till beginning of line
* `M-w` 		Copy current line or region
* `C-y` 		Paste (aka yank)
* `M-y` 		Cycle last paste through previous kills
* `C-x C-y` 	Choose what to paste from previous kills

### General

* `C-g` 		Quit out of whatever mess you've gotten yourself into
* `M-x` 		Run a command by name
* `C-.` 		Auto complete
* `C-_` 		Undo
* `M-_` 		Redo
* `C-x u` 		Show the undo-tree
* `C-x m` 		Open magit. It's a magical git interface for emacs ([cheat sheet](http://daemianmack.com/magit-cheatsheet.html))

### Navigation

* `C-<left,right>` Move over words/paragraphs
* `M-<left,right>` Move over expressions
* `C-<up,down>` Moves buffer up/down without moving point
* `C-a` 		Go to start of line / indentation (press multiple times to toggle between the two)
* `C-e` 		Go to end of line
* `M-g M-g` 	Go to line number
* `C-x C-i` 	Go to symbol
* `C-s` 		Search forward. Press `C-s` again to go further.
* `C-r` 		Search backward. Press `C-r` again to go further.
* `C-S-s`       Search using [ag](https://github.com/ggreer/the_silver_searcher)
* `Home` 		Go to the beginning of the buffer
* `End` 		Go to the end of the buffer

### Window management

* `C-x o` 		Close this window
* `C-x p` 		Close other windows
* `C-x 2` 		Split window horizontally
* `C-x 3` 		Split window vertically
* `S-<up, right, left, down>` Jump to other windows
* `M-C-<tab>`	Rotate the way two windows are split
* `M-S-<tab>`   Rotate windows

### Help

* `F1 t` 		Basic tutorial
* `F1 k` 		Help for a key binding
* `F1 r` 		Emacs' extensive documentation
* `F1 m` 		Help for current mode
