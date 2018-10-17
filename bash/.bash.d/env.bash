
#export MPD_HOST=192.168.42.1

export EMAIL="raskolnikov@gnu.org"
export USERNAME="Juan Pedro Bolívar Puente"

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c"

#
# Utils for manipulating environment paths
#

add-path()
{
    var=$1
    del-path $var $@
    shift
    for path in $@
    do
        declare -gx $var="$path${!var:+":${!var}"}"
    done
}

del-path()
{
    var=$1
    shift
    for path in $@
    do
        declare -gx $var=${!var//":$path:"/:} #delete all instances in the middle
        declare -gx $var=${!var/%":$path"/} #delete any instance at the end
        declare -gx $var=${!var/#"$path:"/} #delete any instance at the beginning
        declare -gx $var=${!var//"$path"/} #delete singleton instance
    done
}

#
# local installations
#
add-path LD_LIBRARY_PATH /usr/local/lib
add-path PATH /usr/local/bin

#
# Guile
#
add-path GUILE_LOAD_PATH . ... $HOME/dev/immer/build/extra/guile
export GUILE_AUTO_COMPILE=1
export GUILE_WARN_DEPRECATED=no

#
# Guix
#

# export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
export GUIX_LD_WRAPPER_ALLOW_IMPURITIES=1
add-path GUIX_PACKAGE_PATH "$HOME/dotfiles/guix"
add-path GUILE_LOAD_PATH "$HOME/dotfiles/guix"
add-path GUILE_LOAD_PATH "$HOME/usr/guile"

if [ -d ~/.guix-profile ]; then
    add-path GUIX_LOCPATH "$HOME/.guix-profile/lib/locale"
    add-path PATH "$HOME/.guix-profile/bin" "$HOME/.guix-profile/sbin"
    add-path GUILE_LOAD_PATH "$HOME/.guix-profile/share/guile/site/2.2"
    add-path GUILE_LOAD_COMPILED_PATH "$HOME/.guix-profile/lib/guile/2.2/site-ccache"
    #add-path LD_LIBRARY_PATH "$HOME/.guix-profile/lib" "/usr/lib/x86_64-linux-gnu" "/usr/lib"
    #add-path LIBRARY_PATH "$HOME/.guix-profile/lib" "/usr/lib" "/usr/lib/x86_64-linux-gnu"
    #add-path CPATH "$HOME/.guix-profile/include" "/usr/include" "/usr/include/x86_64-linux-gnu"
    #add-path C_INCLUDE_PATH "$HOME/.guix-profile/include" "/usr/include" "/usr/include/x86_64-linux-gnu"
    #add-path CPLUS_INCLUDE_PATH "$HOME/.guix-profile/include" "/usr/include" "/usr/include/x86_64-linux-gnu"
    #add-path INFOPATH "$HOME/.guix-profile/share/info"
    #add-path PKG_CONFIG_PATH "$HOME/.guix-profile/lib/pkgconfig"
    #add-path ACLOCAL_PATH "$HOME/.guix-profile/share/aclocal"
    #add-path BASH_LOADABLES_PATH "/home/raskolnikov/.guix-profile/lib/bash"
    #add-path TERMINFO_DIRS "/home/raskolnikov/.guix-profile/share/terminfo"
fi

#
# Nix
#
if [ -z "$NIX_LINK" ]; then
    if [ -f /nix/var/nix/profiles/default/etc/profile.d/nix.sh ]; then
        source /nix/var/nix/profiles/default/etc/profile.d/nix.sh
    fi
    if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
    fi
    add-path NIX_PATH "ssh-config-file=$HOME/.ssh/nix-ssh-config"
fi

export NIX_REMOTE=daemon

#
# General
#
is-macos() {
    [[ "$(uname)" == "Darwin" ]]
}

is-guix-environment() {
    [[ ! -z "$GUIX_ENVIRONMENT" ]]
}

is-nix-shell() {
    [[ ! -z "$NIX_STORE" ]]
}

#
# User installations
#
add-path LD_LIBRARY_PATH "$HOME/usr/lib"
add-path PATH "$HOME/usr/bin"

add-path LD_LIBRARY_PATH "$HOME/.local/lib"
add-path PATH "$HOME/.local/bin"

#
# C++
#
export LCVER=5.0
if is-macos; then
    export LC="/usr/local/opt/llvm/bin/clang-$LCVER"
    export LXX="/usr/local/opt/llvm/bin/clang-$LCVER"
else
    export LC="clang-$LCVER"
    export LXX="clang++-$LCVER"
fi

export GCVER=7
export GC="gcc-$GCVER"
export GXX="g++-$GCVER"
export SHLIB_GXXLD="g++-$GCVER"

clean-cmake() {
    trash ./CMakeCache.txt
    trash ./CMakeFiles
}

maybe-clean-cmake() {
    cdir=`basename $PWD`
    if [ "x$cdir" == "xbuild" ] && \
           [ -f ../CMakeLists.txt ] && \
           [ -f ./CMakeCache.txt ] && \
           [ -d ./CMakeFiles ];
    then
        cmake_cc=$(cat CMakeCache.txt | grep CMAKE_C_COMPILER:FILEPATH)
        cmake_cxx=$(cat CMakeCache.txt | grep CMAKE_CXX_COMPILER:FILEPATH)
        cc=`test -n "$cmake_cc" && basename "$cmake_cc"`
        cxx=`test -n "$cmake_cxx" && basename "$cmake_cxx"`
        if [ "$cc" != "$CC" ] ||  [ "$cxx" != "$CXX" ];
        then
            echo -e "-- CMake cache was using: \tCC=$cc \tCXX=$cxx"
            echo -e "-- Changing compiler to:  \tCC=$CC \tCXX=$CXX"
            trash ./CMakeCache.txt
            trash ./CMakeFiles
        fi
    fi
}

use-default() {
    export CC=cc
    export CXX=c++
    maybe-clean-cmake
}

use-clang() {
    export CC=$LC
    export CXX=$LXX
    maybe-clean-cmake
}

use-gcc() {
    export CC=$GC
    export CXX=$GXX
    maybe-clean-cmake
}

use-guix() {
    export CC=$HOME/.guix-profile/bin/gcc
    export CXX=$HOME/.guix-profile/bin/g++
    maybe-clean-cmake
}

disable-ccache() {
    export CC=${CC#ccache}
    export CXX=${CXX#ccache}
}

use-ccache() {
    disable-ccache
    export CC="ccache $CC"
    export CXX="ccache $CXX"
}

export CTEST_OUTPUT_ON_FAILURE=1

if is-macos; then
    : # noop
elif is-guix-environment; then
    echo "-- setting up guix environment: $GUIX_ENVIRONMENT"
    export CC=$GUIX_ENVIRONMENT/bin/gcc
    export CXX=$GUIX_ENVIRONMENT/bin/c++
else
    export CC=$GC
    export CXX=$GXX
fi

#
# emscripten
#
add-path PATH $HOME/soft/binaryen/build/bin
add-path PATH $HOME/dev/emsdk
#[ -f $HOME/dev/emsdk/emsdk_set_env.sh ] && \
#    source $HOME/dev/emsdk/emsdk_set_env.sh

#
# Go
#
export GOPATH=~/.go-path
add-path PATH $GOPATH/bin

#
# Haskell
#
add-path PATH ~/.cabal/bin

#
# Node.js
#
export NPM_PACKAGES="${HOME}/.npm-packages"
export NODE_PATH="${NPM_PACKAGES}"
add-path PATH "${NPM_PACKAGES}/bin"

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
# Clojure
#
# export LEIN_FAST_TRAMPOLINE=true

#
# Scala
#
export CONSCRIPT_HOME="$HOME/.conscript"
export CONSCRIPT_OPTS="-Dfile.encoding=UTF-8"
add-path PATH $CONSCRIPT_HOME/bin

#
# Ruby
#
add-path PATH $HOME/.gem/ruby/2.3.0/bin
