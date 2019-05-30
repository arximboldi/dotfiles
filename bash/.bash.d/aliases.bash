# bash
alias reload="source ~/.bashrc"

# sudo
alias s=sudo

function script-sudo
{
    if [ "${TERM}" == eterm-color ]; then
        gksudo -- $*
    else
        sudo $*
    fi
}

# colors
if ! is-nix-shell && [ "$(uname)" == "Darwin" ]; then
    alias ls='gls --color=auto -X --group-directories-first'
else
    alias ls='ls --color=auto -X --group-directories-first'
fi
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
alias trall='trash *'
alias trtilde='find . -name "*~" -exec trash {} +'

# Open like from nautilus
if [ "$(uname)" != "Darwin" ]; then
    function open()
    {
        for var in "$@"
        do
            xdg-open "$var"
        done
    }
fi

# git
alias gsuri="git submodule update --recursive --init"
# http://stackoverflow.com/a/11366713/677381
alias gignore="git update-index --assume-unchanged"
alias gunignore="git update-index --no-assume-unchanged"
alias glsignore="git ls-files -v | grep \"^[[:lower:]]\""

# emacs
if [ "$(uname)" == "Darwin" ]; then
    alias ee="emacsclient -n"
else
    function toemacs() { $* && wmctrl -xa emacs; }
    alias ee="toemacs emacsclient -n"
    complete -r ee 2> /dev/null
fi
alias e="emacsclient -t"
alias ew="emacsclient -n -c"
alias se="SUDO_EDITOR='emacsclient -t' sudoedit"
alias see="SUDO_EDITOR='emacsclient' toemacs sudoedit -b $*"
alias sew="SUDO_EDITOR='emacsclient -c' sudoedit -b $*"

alias killemacs="emacsclient -e \"(kill-emacs)\" -a false"
alias compemacs='emacs --batch -l ~/.emacs.d/init.el --eval "(byte-recompile-directory (expand-file-name \"~/.emacs.d\") 0)" --kill'

# cmake
alias cmake="\cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
alias c=cmake
alias cn="cmake -G Ninja"
alias cn-all="cn .. && ninja && ctest --output-on-failure"
alias cm="cmake -G 'Unix Makefiles'"
alias cm-all="cm .. && make -j 4 && ctest --output-on-failure"

# math
alias p="ipython --no-confirm-exit"
alias o=octave-cli

# web dev
alias pjson="python -mjson.tool"

# screen
alias scr="screen -DR"

# email
alias sm="sync-mail"

alias backup="rsync -a -A -X --info=progress2"

# latex
svg2pdf_tex() {
    fname=$1
    fname_noext="${fname%.*}"
    inkscape -D -z --file=$fname --export-pdf=$fname_noext.pdf --export-latex
}

svg2pdf() {
    for fname; do
        fname_noext="${fname%.*}"
        inkscape -D -z --file=$fname --export-pdf=$fname_noext.pdf
    done
}

pa-jack-start()
{
    pactl load-module module-jack-sink channels=2
    pactl load-module module-jack-source channels=2
    pacmd set-default-sink jack_out
    pacmd set-default-source jack_in
}

pa-jack-stop()
{
    SINKID=$(pactl list | grep -B 1 "Name: module-jack-sink" | grep Module | sed 's/[^0-9]//g')
    SOURCEID=$(pactl list | grep -B 1 "Name: module-jack-source" | grep Module | sed 's/[^0-9]//g')
    pactl unload-module $SINKID
    pactl unload-module $SOURCEID
}

gdbwait()
{
    progstr=$1
    progpid=""
    while [ "$progpid" = "" ]; do
        progpid=`pgrep -n $progstr`
    done
    gdb -p $progpid
}

# jack
alias jack-hw="jackd -P70 -t5000 -dalsa -dhw:0 -r44100 -p128 -n2 -s -Xseq -o2"

# guix
alias guix-shell="guix environment --ad-hoc"

function counter-ncurses-meta-meta-meetingcpp17()
{
    mplayer -fs /home/raskolnikov/media/videos/terminator/render3.webm \
            > /dev/null \
            2> /dev/null
}

function fake-spinner()
{
    sp="/-\|"
    echo -n ' '
    echo -n "$*  "
    for i in $(seq 1 20)
    do
        printf "\b${sp:i++%${#sp}:1}"
        sleep 0.1s
    done
    echo
}

function counter-ncurses-meta-meta()
{
    fake-spinner "== activating flux compressor"
    fake-spinner "== gradient descent on worm-hole"
    fake-spinner "== trascending meta-temporal existentiality"
    mplayer -fs /home/raskolnikov/media/videos/cppcon18/video-4.mp4 \
            > /dev/null \
            2> /dev/null
}

[ -f ~/dev/prenav/env.bash ] && \
    source ~/dev/prenav/env.bash
