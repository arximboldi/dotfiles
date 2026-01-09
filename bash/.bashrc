# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

test -e /etc/static/bashrc \
    && . /etc/static/bashrc

source ~/.bash.d/init.bash
