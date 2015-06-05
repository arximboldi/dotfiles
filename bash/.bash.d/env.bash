
export EMAIL="raskolnikov@gnu.org"
export USER="Juan Pedro Bolívar Puente"

export EDITOR="emacsclient -a emacs24"


export LCVER=3.7
export LC="ccache clang-$LCVER"
export LXX="ccache clang++-$LCVER"

export CCVER=4.9
export CC="ccache gcc-$CCVER"
export CXX="ccache g++-$CCVER"
export SHLIB_CXXLD="g++-$CCVER"
# export CCFLAGS="-fdiagnostics-color=always"
# export CXXFLAGS="-fdiagnostics-color=always"

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
