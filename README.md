dotfiles
========

So, here is another place in the Internet where someone is backing up
her configuration files.

To use them, try out using `GNU Stow` as described [in this
article](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html).
Here are some details about the provided configurations.

Most configurations requires packages as installed in `nix/os`.

nixos
-----

Machines are described in `nix/os/flakes.nix`.

First, enable flakes:
```
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
```

Then:
```
sudo nixos-rebuild switch --flake ~/dotfiles/nix/os#my-machine
```

Or more permanently (when hostname matches config name).
```
sudo mv /etc/nixos /etc/nixos.bak
sudo ln -s ~/dotfiles/nix/os /etc/nixos
sudo nixos-rebuild switch
```

emacs
-----

When you run Emacs for the first time you will be missing packages.
Perhaps we should install those via Nix in the future... for now, run
Emacs, ignore, the errors and update the package list and then install
the required packages with:
```
(progn
 (package-refresh-contents)
 (package-upgrade-all)
 (package-install-selected-packages)
 (package-autoremove))
```
Or simply
```
(@update-packages)
```

macos
-----

Some notes:
```
chsh -s
```
https://stackoverflow.com/questions/453236/how-to-set-my-default-shell-on-mac

http://andresabino.com/2015/04/14/codesign-gdb-on-mac-os-x-yosemite-10-10-2/
