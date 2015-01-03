dotfiles
========

So, here is another place in the Internet where someone is backing up
her configuration files.  To use them, try out using `GNU Stow` as
described [in this
article](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html).
Here are some details about the provided configurations.

emacs
-----

Configuration for GNU Emacs 24 from Debian Sid.  You might need to
install extra packages like `chktex`, but I have lost track of all
what you need.

Here are some packages you might need to install via `package-install`
though:

  * auctex-11.87.5
  * clojure-mode-2.1.1
  * coffee-mode-0.4.1.1
  * color-theme-6.6.1
  * dash-2.7.0
  * epl-0.7
  * gtags-3.3
  * ido-ubiquitous-2.10
  * magit-1.2.1
  * pkg-info-0.5
  * projectile-0.10.0
  * rainbow-delimiters-1.3.21
  * s-1.9.0
  * scss-mode-0.5.0
  * smex-3.0
  * yasnippet-0.8.0

xmonad
------

This is the configuration for `xmonad`.  It also requires a very
recent `taffybar`, that you can just grab [from
upstream](https://github.com/travitch/taffybar).  Check `xmonad.hs` to
figure out all the other dependencies, like `suckless-tools`,
`cinnamon`, among others.

Oh, and this theme looks very well in combination with [the Numix
theme](https://numixproject.org/).
