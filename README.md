# How it looks
![Screenshot of this emacs in action](emacs-screen.png?raw=true "Screenshot of this emacs in action")

# Using this configuration
I recommend starting out with a blank emacs and then digging through this repo for useful
snippets, instead of using it directly. I have heavily configured my emacs, many
default bindings have changed, and behaviour has changed for many packages to better fit
the way I work.

It should however be possible to simply clone the entire repo, drop it
into a clean install of emacs, and it should set everything up
properly, that's how I use it when I need to get something done on a
new machine.

# Notes
I usually rebuild emacs from master once a month, so some things will probably not work on
older emacsen.

Most configuration takes place in `init.el`, it tries to be concise, but have enough comments
to make it easy to grok.

I run emacs using the daemon, so start-up time is not a big concern, this config usually
starts within 2-6 seconds. I very rarely start emacs from cold, I do however want the
config to be snappy, and I go out of my way to try and improve performance for common
tasks.
