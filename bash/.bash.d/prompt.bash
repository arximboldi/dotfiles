
source ~/.bash.d/git-prompt.sh

set_prompt () {
    lastcmd=$? # Must come first!
    blue='\[\e[01;34m\]'
    __blue='\033[34m'
    white='\[\e[01;37m\]'
    red='\[\e[01;31m\]'
    green='\[\e[01;32m\]'
    reset='\[\e[00m\]'
    fancyx='\342\234\227'
    checkmark='\342\234\223'
    PS1=""
    if [[ $EUID == 0 ]]; then
        PS1+="$red\\h"
    else
        PS1+="$green\\u$reset\[$__green\]@\\h"
    fi
    PS1+="$reset \[$__blue\]\\w$red"
    if [[ $EUID != 0 ]]; then
        PS1+='\[`
         case "$(__git_prompt_color)" in
            red) printf '$__red';;
            green) printf '$__green';;
            black) printf '$__black';;
         esac`\]$(__git_prompt)\[\033[0m\]'
    fi
    PS1+=" "
}
PROMPT_COMMAND='set_prompt'
