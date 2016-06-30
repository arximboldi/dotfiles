
source ~/.bash.d/git-prompt.sh

set_prompt () {
    lastcmd=$? # Must come first!

    fancyx='\342\234\227'
    checkmark='\342\234\223'

    purple="\[$(tput bold; tput setaf 5)\]"
    blue="\[$(tput bold; tput setaf 4)\]"
    white="\[$(tput bold; tput setaf 7)\]"
    red="\[$(tput bold; tput setaf 1)\]"
    green="\[$(tput bold; tput setaf 2)\]"

    blue2="\[$(tput setaf 4)\]"
    green2="\[$(tput setaf 2)\]"

    reset="\[$(tput sgr0)\]"

    PS1=""

    if [[ $lastcmd == 0 ]]; then
        PS1+="$green$checkmark$reset "
    else
        PS1+="$red$fancyx$reset "
    fi

    if [ -n "$SSH_CLIENT" ]; then
        PS1+="$red\\h $reset"
    fi

    PS1+="`if [[ $VIRTUAL_ENV != "" ]]; then
              printf $purple;
              printf $(basename $VIRTUAL_ENV);
              printf " "
              printf $reset;
              fi`"

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
