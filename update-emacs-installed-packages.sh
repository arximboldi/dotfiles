#!/bin/sh
emacs24 --batch \
        -l ~/.emacs.d/init.el \
        --eval "(print package-activated-list)" \
        > emacs-installed-packages
