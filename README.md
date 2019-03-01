# The look
![Screenshot of my emacs in action](emacs-screen.png?raw=true "Screenshot of my emacs in action")

# My emacs
I recommend starting out with a blank emacs and then digging through this repo
for useful nuggets, instead of forking it directly.

It should however be possible to simply clone the entire repo, drop it
into a clean install of emacs, and it should set everything up
properly, that's how I use it when I need to get something done on a
new machine.

The keybindings are optimized for a
[danish keyboard layout](http://fontmeme.com/images/danish-keyboard-550x183.png).

All configuration takes place in
[init.el](https://github.com/jensecj/.emacs.d/blob/master/init.el), it
tries to be concise, but have enough comments to make it easy to grok.

You quit emacs with `C-x r q`, mnemonic for *Really Quit*.

I use this in conjunction with the emacs daemon, so when I want to kill the
entire thing, I run `(kill-emacs)` bound to `C-x r k` *Really Kill*.

I symlink this script to `/usr/bin/em`:

`exec /usr/bin/env emacsclient -c -a "" $*`

Then use `em` to run emacs, the first time it is run it
starts the daemon, otherwise it starts a new client and connects
to the daemon.
