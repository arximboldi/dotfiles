# bash
alias reload="source ~/.bashrc"

# sudo
alias s=sudo

# colors
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# ls
alias ll='ls --color=auto -lh'
alias la='ls --color=auto -A'
alias l='ls --color=auto -CF'

# rm
alias rma='rimraf *'

# Open like from nautilus
alias open=xdg-open

# git
alias gsuri="git submodule update --recursive --init"

# emacs
alias e='emacsclient -a emacs24'
alias emacs='emacsclient -a emacs24'
alias se='SUDO_EDITOR=\"emacsclient -c -a emacs" sudoedit'

# cmake
alias cmnt="cmake -G Ninja .. && ninja && ctest -V"
alias cmn="cmake -G Ninja .."

# ipython
alias p=ipython

# ableton
alias abl-configure="modules/build-system/scripts/configure.py"
alias abl-build="modules/build-system/scripts/build.py"
alias abl-run="modules/build-system/scripts/run.py"
alias abl-all="abl-configure && abl-build && abl-run"
