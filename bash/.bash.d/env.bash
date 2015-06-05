
export EMAIL="raskolnikov@gnu.org"
export USER="Juan Pedro Bolívar Puente"

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
