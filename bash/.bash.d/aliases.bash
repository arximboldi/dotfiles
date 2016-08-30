# bash
alias reload="source ~/.bashrc"

# sudo
alias s=sudo

# colors
alias ls='ls --color=auto -X --group-directories-first'
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
alias rm='echo "This is not the command you are looking for."; false'
alias tra='trash *'

# Open like from nautilus
function open()
{
    for var in "$@"
    do
        xdg-open $var
    done
}

# git
alias gsuri="git submodule update --recursive --init"
# http://stackoverflow.com/a/11366713/677381
alias gignore="git update-index --assume-unchanged"
alias gunignore="git update-index --no-assume-unchanged"
alias glsignore="git ls-files -v | grep \"^[[:lower:]]\""

# emacs
function toemacs() { $* && wmctrl -xa emacs; }

alias e="emacsclient -t"
alias ee="toemacs emacsclient -n"
alias ew="emacsclient -n -c"
alias se="SUDO_EDITOR='emacsclient -t' sudoedit"
alias see="SUDO_EDITOR='emacsclient' toemacs sudoedit -b $*"
alias sew="SUDO_EDITOR='emacsclient -c' sudoedit -b $*"
complete -r ee

alias killemacs="emacsclient -e \"(kill-emacs)\" -a false"

# cmake
alias cmake="/usr/bin/cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
alias cmnt="cmake -G Ninja .. && ninja && ctest --output-on-failure"
alias cmn="cmake -G Ninja .."
alias cmmt="cmake -G 'Unix Makefiles' .. && make -j 4 && ctest --output-on-failure"
alias cmm="cmake -G 'Unix Makefiles' .."

# ipython
alias p=ipython

# ableton
alias abl-configure="modules/build-system/scripts/configure.py"
alias abl-build="modules/build-system/scripts/build.py"
alias abl-run="modules/build-system/scripts/run.py"
alias abl-all="abl-configure && abl-build && abl-run"

# web dev
alias pjson="python -mjson.tool"

# screen
alias scr="screen -DR"
