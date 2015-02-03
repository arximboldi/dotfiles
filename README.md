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

  *  async-20141229.2221
  *  auctex-11.88.2
  *  cljsbuild-mode-20140619.126
  *  clojure-mode-20150113.1048
  *  clojurescript-mode-0.5
  *  coffee-mode-20150130.1627
  *  color-theme-20080305.34
  *  dash-20141220.1452
  *  diminish-20091203.1012
  *  dirtrack-buffer-name-track-mode-1.0.0
  *  epl-20140823.609
  *  flymake-easy-20140818.55
  *  flymake-sass-20140308.325
  *  frame-cmds-20150104.2207
  *  frame-fns-20150104.2208
  *  git-commit-mode-20141014.1634
  *  git-messenger-20140923.738
  *  git-rebase-mode-20150122.1114
  *  gotham-theme-20150202.1357
  *  gratuitous-dark-theme-1.3
  *  gruber-darker-theme-20141010.105
  *  gtags-3.3
  *  haml-mode-20141213.920
  *  helm-20150203.36
  *  helm-projectile-20150120.2256
  *  helm-themes-20141117.740
  *  ido-ubiquitous-20141210.1353
  *  magit-90150116
  *  magit-filenotify-20150125.1456
  *  mediawiki-20130223.1141
  *  multi-term-20141203.1658
  *  pkg-info-20140610.630
  *  popup-20150116.1223
  *  projectile-20150201.1134
  *  rainbow-delimiters-20141221.925
  *  request-20140316.417
  *  s-20140910.334
  *  sass-mode-20141219.324
  *  scss-mode-20150107.1400
  *  smex-20141210.1422
  *  travis-20141222.203
  *  xterm-color-20130904.1826
  *  yasnippet-20141223.303
  *  zoom-frm-20150110.2052
  *  zoom-window-20141204.546

xmonad
------

This is the configuration for `xmonad`.  It also requires a very
recent `taffybar`, that you can just grab [from
upstream](https://github.com/travitch/taffybar).  Check `xmonad.hs` to
figure out all the other dependencies, like `suckless-tools`,
`cinnamon`, among others.

It also depends on
[wallpaperd](https://projects.pekdon.net/projects/wallpaperd) which
may be installed separatelly.

Oh, and this theme looks very well in combination with [the Numix
theme](https://numixproject.org/).

bash
----

Here is some bash stuff that enables `git` niceties among other
things.
