
export MPD_HOST=192.168.42.1

export EMAIL="raskolnikov@gnu.org"
export USERNAME="Juan Pedro Bolívar Puente"

export EDITOR="emacsclient -a emacs24"

export LCVER=3.8
export LC="ccache clang-$LCVER"
export LXX="ccache clang++-$LCVER"

export GCVER=5
export GC="ccache gcc-$GCVER"
export GXX="ccache g++-$GCVER"
export SHLIB_GXXLD="g++-$GCVER"

# export CCFLAGS="-fdiagnostics-color=always"
# export CXXFLAGS="-fdiagnostics-color=always"

export GOPATH=~/.go-path

add-path $GOPATH/bin
add-path /home/raskolnikov/usr/bin
add-path /home/raskolnikov/.cabal/bin

export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules

# Google cloud stuff
[ -f /home/raskolnikov/.travis/travis.sh ] && \
    source ~/soft/google-cloud-sdk/path.bash.inc
[ -f ~/soft/google-cloud-sdk/completion.bash.inc ] && \
    source ~/soft/google-cloud-sdk/completion.bash.inc
[ -f ~/.travis/travis.sh ] && \
    source ~/.travis/travis.sh

# Guix
export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
add-path /home/raskolnikov/.guix-profile/bin
