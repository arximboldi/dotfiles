
source ~/.bash.d/git-prompt.sh

set_prompt () {
    lastcmd=$? # Must come first!

    fancyx='\342\234\227'
    checkmark='\342\234\223'

    purple='\[\e[01;35m\]'
    blue='\[\e[01;34m\]'
    blue2='\[\e[00;34m\]'
    white='\[\e[01;37m\]'
    red='\[\e[01;31m\]'
    green='\[\e[01;32m\]'
    green2='\[\e[00;32m\]'
    reset='\[\e[00m\]'

    PS1=""

    if [[ $lastcmd == 0 ]]; then
        PS1+="$green$checkmark$reset "
    else
        PS1+="$red$fancyx$reset "
    fi

    if [ -n "$SSH_CLIENT" ]; then
        PS1+="$red\\h $reset"
    fi

    if [[ $EUID == 0 ]]; then
        PS1+="$red\\u$reset"
    elif [[ $GUIX_ENVIRONMENT == "t" ]]; then
        PS1+="$purple\\u$reset"
    else
        PS1+="$green2\\u$reset"
    fi

    PS1+=" $blue2\\w$reset"

    if [[ $EUID != 0 ]]; then
        PS1+="`
         case "$(__git_prompt_color)" in
            red) printf "$red";;
            green) printf "$green";;
            black) printf "$white";;
         esac`$(__git_prompt)$reset"
    fi

    PS1+=" "
}

PROMPT_COMMAND='set_prompt'
