# My emacs settings

Heavily influenced by [magnars](https://github.com/magnars/.emacs.d) and [cjohansen](https://github.com/cjohansen/.emacs.d).

## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * I recommend starting with a blank emacs + [Technomancy's better-defaults package](https://github.com/technomancy/better-defaults),
   and then dig through this repo for useful nuggets, instead of forking it directly.

 * It should be possible to simply clone the entire repo, drop it into a clean 
   install of emacs, and it should set everything up properly, but your experience may vary!

 * The key bindings are optimized for a [danish keyboard layout](http://fontmeme.com/images/danish-keyboard-550x183.png).

 * Start by reading up on all the cool stuff in key-bindings.el.

 * You quit emacs with `C-x r q`, mnemonic *Really Quit*.

### Custom keybindings

* `M-j`         Joins the line below to this line
* `C-c C-k`     Evaluate current buffer
* `C-c k`       Evaluate current region
* `C-c n`       Indent and clean up white space in the entire buffer

### Ace-jump-mode

* `C-ø` 		Quickly jump to the start of any word in the buffer
* `C-Ø` 		Quickly jump to any character inside a word
* `C-'` 		Quickly jump to any line

### Multiple-cursors

* `C-d` 		To put a new cursor at the next thing like the current selection
* `C-S-d` 		To put a new cursor at all things like the current selection
* `C-M-a`		Drops a rectangular anchor, then use up and down to move

### Expand-region

* `M-e`			Expand onto the next region
* `C-M-e`		Contracts back to the previous region

### Smart-forward

* `M-arrow` 	up, down, left or right to move onto the next region

### Move-text

* `C-S-arrow` 	up or down to move a line up or down

### Change-inner

* `M-i` 	    copy contents of inner selection
* `M-o` 	    copy contents of outer selection
* `M-I` 	    change contents of inner selection
* `M-O` 	    change contents of outer selection

### Files

* `C-x C-f` 	Open a file. Starts in the current directory
* `C-x f` 		Open a recently visited file
* `C-x o` 		Open a file in the current project (based on .git)
* `C-x C-s` 	Save this file
* `C-x C-w` 	Save as ...
* `C-x t` 	    Force save this file
* `C-x C-j` 	Jump to this files' current directory
* `C-x b` 		Switch to another open file (buffer)
* `C-x C-b` 	List all open files (buffers)

### Cut copy and paste

* `C-space` 	Start marking stuff. C-g to cancel.
* `C-w` 		Cut (aka kill)
* `C-k` 		Cut till end of line
* `M-w` 		Copy current line / region
* `C-y` 		Paste (aka yank)
* `M-y` 		Cycle last paste through previous kills
* `C-x C-y` 	Choose what to paste from previous kills

### General

* `C-g` 		Quit out of whatever mess you've gotten yourself into
* `M-x` 		Run a command by name
* `C-.` 		Autocomplete
* `C-_` 		Undo
* `M-_` 		Redo
* `C-x u` 		Show the undo-tree
* `C-x m` 		Open magit. It's a magical git interface for emacs ([cheatsheet](http://daemianmack.com/magit-cheatsheet.html))

### Navigation

* `C-<left,right>` Move past words/paragraphs, up, down - moves buffer up/down without moving point
* `C-<up,down>` Moves buffer up/down without moving point
* `C-a` 		Go to start of line
* `C-e` 		Go to end of line
* `M-g M-g` 	Go to line number
* `C-x C-i` 	Go to symbol
* `C-s` 		Search forward. Press `C-s` again to go further.
* `C-r` 		Search backward. Press `C-r` again to go further.

### Window management

* `C-x 0` 		Close this window
* `C-x 1` 		Close other windows
* `C-x 2` 		Split window horizontally
* `C-x 3` 		Split window vertically
* `S-arrow`		Jump to window to the left/right/up/down

### Help

* `F1 t` 		Basic tutorial
* `F1 k` 		Help for a keybinding
* `F1 r` 		Emacs' extensive documentation
