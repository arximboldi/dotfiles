# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Make emacs realize it can use colors
if [ "x$EMACS" == "xt" ]; then
    export TERM=xterm
fi

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+:}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
# [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (may be used in the
# prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || \
        eval "$(dircolors -b)"
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
export CC=colorgcc

# some more ls aliases
alias ll='ls --color=auto -lh'
alias la='ls --color=auto -A'
alias l='ls --color=auto -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

GIT_PROMPT=~/usr/bin/git-prompt.sh
GIT_COMPLETION=~/usr/bin/git-completion.bash
source $GIT_PROMPT
source $GIT_COMPLETION

set_prompt () {
    lastcmd=$? # Must come first!
    blue='\[\e[01;34m\]'
    __blue='\033[34m'
    white='\[\e[01;37m\]'
    red='\[\e[01;31m\]'
    green='\[\e[01;32m\]'
    reset='\[\e[00m\]'
    fancyx='\342\234\227'
    checkmark='\342\234\223'
    PS1=""
    if [[ $EUID == 0 ]]; then
        PS1+="$red\\h"
    else
        PS1+="$green\\u$reset\[$__green\]@\\h"
    fi
    PS1+="$reset \[$__blue\]\\w$red"
    if [[ $EUID != 0 ]]; then
        PS1+='\[`
         case "$(__git_prompt_color)" in
            red) printf '$__red';;
            green) printf '$__green';;
            black) printf '$__black';;
         esac`\]$(__git_prompt)\[\033[0m\]'
    fi
    PS1+=" "
}
PROMPT_COMMAND='set_prompt'

export PATH=/home/raskolnikov/usr/bin:$PATH
export PATH=/home/raskolnikov/.cabal/bin:$PATH
export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules

# Google cloud stuff
source ~/soft/google-cloud-sdk/path.bash.inc
source ~/soft/google-cloud-sdk/completion.bash.inc
