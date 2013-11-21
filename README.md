# My emacs settings

Heavily influenced by [magnars](https://github.com/magnars/.emacs.d) and [cjohansen](https://github.com/cjohansen/.emacs.d)

## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * I recommend starting with a blank emacs +
   [Technomancy's better-defaults package](https://github.com/technomancy/better-defaults),
   and then dig through this repo for useful nuggets, instead of forking it directly.

 * It should be possible to simply clone entire repo, drop it into a clean 
   install of emacs, and it should set everything up properly, but bugs may exist!

 * The key bindings are optimized for a [danish keyboard layout](http://fontmeme.com/images/danish-keyboard-550x183.png).

 * Start by reading up on all the cool stuff in key-bindings.el.

 * You quit emacs with `C-x r q`, mnemonic *Really Quit*.

 * Find file in dir with `C-x C-f`, recent files with `C-x f`

 * `C-h` is rebound to backspace, like in the shell. Get help on `F1` instead.

 * Undo with `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

 * Indent and clean up white space in the entire buffer with `C-c n`

### Ace-jump-mode

* `C-ø` 		Quickly jump to the start of any word in the buffer
* `C-Ø` 		Quickly jump to any character inside a word
* `C-'` 		Quickly jump to any line

### Multiple-cursors

* `C-d` 		To put a new cursor at the next thing like the current selection
* `C-S-d` 		To put a new cursor at all things like the current selection
* `C-M-a`		Drops a rectangular anchor, then use <up> and <down> to move

### expand-region

* `M-e`			Expand onto the next region
* `C-M-e`		Contracts back to the previous region

### smart-forward

* `M-<arrow>' 	<up>, <down>, <left> or <right> to move onto the next region

### move-text

* `C-S-<arrow>' <up> or <down> to move a line up or down

### Files

* `C-x C-f` 	Open a file. Starts in the current directory
* `C-x f` 		Open a recently visited file
* `C-x o` 		Open a file in the current project (based on .git)
* `C-x C-s` 	Save this file
* `C-x C-w` 	Save as ...
* `C-x C-j` 	Jump to this files' current directory
* `C-x b` 		Switch to another open file (buffer)
* `C-x C-b` 	List all open files (buffers)

### Cut copy and paste

* `C-space` 	Start marking stuff. C-g to cancel.
* `C-w` 		Cut (aka kill)
* `C-k` 		Cut till end of line
* `M-w` 		Copy
* `C-y` 		Paste (aka yank)
* `M-y` 		Cycle last paste through previous kills
* `C-x C-y` 	Choose what to paste from previous kills
* `C-@`		 	Mark stuff quickly. Press multiple times

### General

* `C-g` 		Quit out of whatever mess you've gotten yourself into
* `M-x` 		Run a command by name
* `C-.` 		Autocomplete
* `C-_` 		Undo
* `M-_` 		Redo
* `C-x u` 		Show the undo-tree
* `C-x m` 		Open magit. It's a magical git interface for emacs

### Navigation

* `C-arrow` 	Move past words/paragraphs
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
