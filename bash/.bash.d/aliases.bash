# bash
alias reload="source ~/.bashrc"

# sudo
alias s=sudo

alias girl="echo I love you"

function script-sudo
{
    if [ "${TERM}" == eterm-color ]; then
        gksudo -- $*
    else
        sudo $*
    fi
}

# colors
if ! is-nix-shell && [ "$(uname)" == "Darwin" ] && ! [ "$(which ls)" == "/run/current-system/sw/bin/ls" ]; then
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

alias rsync-backup="rsync -a -A -X --info=progress2"

# webcam
alias photo="echo wait 3s... && sleep 1s && echo wait 2s... && sleep 1s && echo wait 1s... && sleep 1s && fswebcam --no-banner -r 1920x1080 --save photo-$(date -Iseconds).png"

# download music from internet
alias ripz="yt-dlp -x --audio-format mp3 --audio-quality 0"

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

alias pa-hdmi="pactl set-card-profile 0 output:hdmi-stereo+input:analog-stereo"
alias pa-normal="pactl set-card-profile 0 output:analog-stereo+input:analog-stereo"

# run video2x
alias video2x-podman='sudo podman run --runtime nvidia --gpus all --privileged --rm -it -v "$PWD":/host docker.io/arximboldi/video2x-esrgan:latest'

function check-upscale-resolution()
{
    min_height="$1"
    target_height="$2"
    file="$3"
    if ! test -e "$file"; then
        echo "> pass an existing file as second argument" >&2;
        return 1
    fi
    if ! expr "$min_height" + 0 &>/dev/null; then
        echo "> pass the minimum height as first argument" >&2;
        return 1
    fi
    if ! expr "$target_height" + 0 &>/dev/null; then
        echo "> pass the target height as second argument" >&2;
        return 1
    fi
    res=$(ffprobe -v error \
                  -select_streams v:0 \
                  -show_entries stream=width,height \
                  -of csv=s=x:p=0 "$file")
    width=$(echo $res | cut -d'x' -f1)
    height=$(echo $res | cut -d'x' -f2)
    if ! expr "$width" + 0 &>/dev/null; then
        echo "> couldn't get resolution for ${file}" >&2
        return 1
    fi
    if ! expr "$height" + 0 &>/dev/null; then
        echo "> couldn't get resolution for ${file}" >&2
        return 1
    fi
    echo "resolution ${width}x${height} found for ${file}" >&2
    if [ "$height" -ge "$min_height" ]; then
        echo "> skipping..."
        return 1
    else
        # out variable
        target_width=$(($width * $target_height / $height))
        echo "> upscaling to ${target_width}x${target_height}..." >&2
        return 0
    fi
}

function video2x-local()
{
    nix-shell ~/dev/video2x/shell.nix --argstr command "pdm run -p ~/dev/video2x python -m video2x -l debug $*"
}

# returns via variables out_file and in_file
function check-upscale-files()
{
    if [ -z "$1" ]
    then
        echo "> need to pass an input file" >&2
        return 1
    fi

    if [ -z "$3" ]
    then
        echo "> need to pass an appendix" >&2
        return 1
    fi

    appendix=$3
    in_file=$1
    in_file_name="${in_file%.*}"
    in_file_ext="${in_file##*.}"

    if [[ "$in_file" == *.upscaled.* ]];
    then
        echo "> file already converted: $in_file" >&2
        return 1
    fi

    # used to be $in_file_ext, but keeping the container format can
    # cause problems with .ogm, etc. which don't support h.264
    out_file_ext="mkv"
    default_out_file="${in_file_name}.upscaled.${appendix}.${out_file_ext}"
    out_file="${2:-${default_out_file}}"

    if [ -e "$out_file" ]
    then
        echo "> output file already exists: $out_file" >&2
        return 1
    fi
    if ! [ -e "$in_file" ]
    then
        echo "> input file does not exists: $in_file" >&2
        return 1
    fi
}

function anime4k()
{
    echo "> ---"
    in_file=
    out_file=
    target_width=
    target_height=1080
    min_height=720
    if ! check-upscale-files "$1" "$2" "anime4k-${target_height}.tmp"; then
        return 1;
    fi
    tmp_file=$out_file
    if ! check-upscale-files "$1" "$2" "anime4k-${target_height}"; then
        return 1;
    fi
    echo "> input:  $in_file" >&2
    echo "> output: $out_file" >&2
    if ! check-upscale-resolution "$min_height" "$target_height" "$in_file"
    then
        return 1;
    fi
    # encode using mpv via shaders
    if mpv "$in_file" \
           --no-sub \
           --glsl-shaders="~~/shaders/Anime4K_Clamp_Highlights.glsl:~~/shaders/Anime4K_Restore_CNN_VL.glsl:~~/shaders/Anime4K_Upscale_CNN_x2_VL.glsl:~~/shaders/Anime4K_Restore_CNN_M.glsl:~~/shaders/Anime4K_AutoDownscalePre_x2.glsl:~~/shaders/Anime4K_AutoDownscalePre_x4.glsl:~~/shaders/Anime4K_Upscale_CNN_x2_M.glsl" \
           -vf=gpu="w=${target_width}:h=${target_height}" \
           --o="$tmp_file" \
           --audio=no
    then
        # copy audio, subs, data, etc. from the original file
        ffmpeg -i "$tmp_file" -i "$in_file" -c copy \
               -map 0:v \
               -map 1:a? -map 1:s? -map 1:d? -map 1:d? \
               -max_interleave_delta 0 \
               "$out_file"
    fi
    \rm -f "$tmp_file"
}

function check-video2x-algorithm()
{
    case "$1" in
        waifu2x) return 0
                 ;;
        srmd) return 0
              ;;
        realsr) return 0
                ;;
        realcugan) return 0
                   ;;
        anime4k) return 0
                 ;;
        realesr-animevideov3) return 0
                              ;;
        realesrgan-x4plus-anime) return 0
                                 ;;
        realesrgan-x4plu) return 0
                          ;;
    esac
    echo "bad algorithm: $1" >&2
    echo "must be one of: " >&2
    echo "waifu2x srmd realsr realcugan anime4k realesr-animevideov3" >&2
    echo "realesrgan-x4plus-anime realesrgan-x4plus" >&2
    return 1
}

function video2x()
{
    echo "> ---"
    target_width=
    target_height=1080
    min_height=720
    algorithm=$1
    if ! check-video2x-algorithm "$algorithm"; then
        return 1
    fi
    in_file=
    out_file=
    if ! check-upscale-files "$2" "$3" "${algorithm}-${target_height}"; then
        return 1
    fi
    echo "> input:  $in_file" >&2
    echo "> output: $out_file" >&2
    if check-upscale-resolution "$min_height" "$target_height" "$in_file"
    then
        video2x-local -i \'"$in_file"\' -o \'"$out_file"\' -p 3 upscale -h "$target_height" -a "$algorithm"
    fi
}

function find-videos() {
    find . -type f \( \
         -iname \*.mp4 -o -iname \*.mkv -o -iname \*.avi -o -iname \*.mov -o \
         -iname \*.flv -o -iname \*.wmv -o -iname \*.m4v -o -iname \*.webm -o \
         -iname \*.mpeg -o -iname \*.mpg -o -iname \*.ogm \
         \)
}

function anime4k-all()
{
    find-videos | while read in_file
    do
        [ -e "$in_file" ] || continue;
        anime4k "$in_file"
    done
}

function video2x-all()
{
    algorithm=$1
    if ! check-video2x-algorithm "$algorithm"; then
        return 1
    fi
    find-videos | while read in_file
    do
        [ -e "$in_file" ] || continue
        video2x "$algorithm" "$in_file"
    done
}

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

[ -f ~/dev/magnopus/automaton/setup-docker.bash ] && \
    source ~/dev/magnopus/automaton/setup-docker.bash
