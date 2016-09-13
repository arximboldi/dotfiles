#
# Make bash history work on multiple tabs and be very big.
# http://unix.stackexchange.com/a/48116
#

HISTSIZE=100000
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignorespace:ignoredups

bash_history_sync() {
    builtin history -a
    HISTFILESIZE=$HISTSIZE
    builtin history -c
    builtin history -r
}

history() {
    bash_history_sync
    builtin history "$@"
}

PROMPT_COMMAND=bash_history_sync
