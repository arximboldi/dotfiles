
export MPD_HOST=192.168.42.1

export EMAIL="raskolnikov@gnu.org"
export USERNAME="Juan Pedro Bolívar Puente"

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"

#
# C++
#
export LCVER=3.8
export LC="ccache clang-$LCVER"
export LXX="ccache clang++-$LCVER"

export GCVER=5
export GC="ccache gcc-$GCVER"
export GXX="ccache g++-$GCVER"
export SHLIB_GXXLD="g++-$GCVER"

use-clang() {
    export CC=$LC
    export CXX=$LXX
}

use-gcc() {
    export CC=$GC
    export CXX=$GXX
}

use-gcc

# export CCFLAGS="-fdiagnostics-color=always"
# export CXXFLAGS="-fdiagnostics-color=always"

export CTEST_OUTPUT_ON_FAILURE=1


#
# emscripten
#
add-path $HOME/soft/binaryen/build/bin
add-path $HOME/dev/emsdk
[ -f $HOME/dev/emsdk/emsdk_set_env.sh ] && \
    source $HOME/dev/emsdk/emsdk_set_env.sh

#
# Go
#
export GOPATH=~/.go-path
add-path $GOPATH/bin

#
# Local installs
#
add-path ~/usr/bin
add-path ~/.local/bin

#
# Haskell
#
add-path ~/.cabal/bin

#
# Node.js
#
export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules

#
# Google cloud stuff
#
[ -f ~/soft/google-cloud-sdk/completion.bash.inc ] && \
    source ~/soft/google-cloud-sdk/completion.bash.inc
[ -f ~/soft/google-cloud-sdk/path.bash.inc ] && \
    source ~/soft/google-cloud-sdk/path.bash.inc

#
# Travis
#
[ -f ~/.travis/travis.sh ] && \
    source ~/.travis/travis.sh

#
# Guix
#
export CPATH=$HOME/.guix-profile/include
export LIBRARY_PATH=$HOME/.guix-profile/lib
export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
export GUILE_LOAD_PATH=$HOME/.guix-profile/share/guile/site/2.0
export GUILE_LOAD_COMPILED_PATH=$HOME/.guix-profile/share/guile/site/2.0
add-path $HOME/.guix-profile/bin
add-path $HOME/.guix-profile/sbin

#
# Clojure
#
# export LEIN_FAST_TRAMPOLINE=true
