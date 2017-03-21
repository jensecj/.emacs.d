# The look
![Screenshot of my emacs in action](emacs-screen.png?raw=true "Screenshot of my emacs in action")
# My emacs

This was originally a fork of
[magnars' great emacs settings](https://github.com/magnars/.emacs.d)
but has since then evolved into a beast better fitting my own
preferences.

I would recommend starting out with a blank emacs and then dig through
this repo for useful nuggets, instead of forking it directly.

It should however be possible to simply clone the entire repo, drop it
into a clean install of emacs, and it should set everything up
properly, that's how I use it when I need to get something done on a
new machine.

The key bindings are optimized for a
[danish keyboard layout](http://fontmeme.com/images/danish-keyboard-550x183.png).

You can check out the package settings and keybindings if you look
through init-package-use-packages.el and init-keybindings.el, from
lisp/.

You quit emacs with `C-x r q`, mnemonic for *Really Quit*. Although I
use `C-x C-c` to kill the frame I'm in, since I leave the emacs server
running indefinitely.

The help key is remapped to `F1`, so `F1 m` for mode help, etc.
