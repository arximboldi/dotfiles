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
though, see the file `emacs-installed-packages`.

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

After installing the configuration, you may need to do:

> ```
> $ cd ~/.themes/Numix
> $ make
> ```

> ```
> sudo apt install libxtst-dev libxkbcommon-dev
> cd _deps/xdotool
> make
> sudo make install
> ```

bash
----

Here is some bash stuff.  Now it is all modularized in a `.bash.d`
folder, with `.bash.d/init.bash` being the entry point.

git
---

Git configuration too, yeah!

macos
-----

Note:
```
chsh -s
```
https://stackoverflow.com/questions/453236/how-to-set-my-default-shell-on-mac

http://andresabino.com/2015/04/14/codesign-gdb-on-mac-os-x-yosemite-10-10-2/
