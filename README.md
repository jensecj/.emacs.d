# How it looks
![Screenshot of my emacs in action](emacs-screen.png?raw=true "Screenshot of my emacs in action")

# Using this configuration
I recommend starting out with a blank emacs and then digging through this repo
for useful nuggets, instead of forking it directly.

It should however be possible to simply clone the entire repo, drop it
into a clean install of emacs, and it should set everything up
properly, that's how I use it when I need to get something done on a
new machine.

# Notes
I usually use the current master release of Emacs, so some things may
not work on older emacsen.

The keybindings are optimized for a
[danish keyboard layout](http://fontmeme.com/images/danish-keyboard-550x183.png).

All configuration takes place in
[init.el](https://github.com/jensecj/.emacs.d/blob/master/init.el), it
tries to be concise, but have enough comments to make it easy to grok.

You quit emacs with `C-x r q`, mnemonic for *Really Quit*.

I use this in conjunction with the emacs daemon, so when I want to kill the
entire thing, I run `(kill-emacs)` bound to `C-x r k` (*Really Kill*).

To run emacs i use the following script:

`exec /usr/bin/env emacsclient -c -a "" $*`

Symlinked to `/usr/local/bin/em`:

Then use `em` to run emacs, the first time it is run it starts the
daemon, then it starts a new client-frame and connects to the daemon.

My emacs config usually starts within 2-5 seconds, and I don't mind it
that way. I very rarely start emacs from cold, I do however want the
config to be snappy, and I go out of my way to try and improve
performance for common tasks.
