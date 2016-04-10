# Make emacs realize it can use colors
if [ "x$EMACS" == "xt" ]; then
    export TERM=xterm
fi

# Use fancy globs
shopt -s extglob

# don't put duplicate lines in the history
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (may be used in the
# prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || \
        eval "$(dircolors -b)"

# enable programmable completion features
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    source /etc/bash_completion
fi

# Include other customization points
source ~/.bash.d/git-completion.bash
source ~/.bash.d/functions.bash
source ~/.bash.d/aliases.bash
source ~/.bash.d/env.bash
source ~/.bash.d/prompt.bash
