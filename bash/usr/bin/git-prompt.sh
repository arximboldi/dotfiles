# A shell prompt that displays some status information about git; in
# particular, it shows what branch you are on, whether your working copy
# is dirty (red) or clean (green), and whether you have something to pull
# or push.
#
# To use it, simply source this file from your ~/.bashrc (or ~/.zshrc if
# you are using zsh) by putting the following line at the end of ~/.bashrc:
#
#   source /path/to/trunk/Tools/Git/Shell-Prompt/git-prompt.sh
#
# If you'd like to customize your prompt, you can redefine PS1 to something
# else after sourcing this script; see the end of this file for an example.
# Let me know if you need help.

if [ "$(locale charmap 2>/dev/null)" = "UTF-8" ]; then
  __arrow_up="\xe2\x86\x91"
  __arrow_down="\xe2\x86\x93"
else
  __arrow_up="<"
  __arrow_down=">"
fi

__black='\033[0m'
__red='\033[31m'
__green='\033[32m'
__tab=`printf '\t'`


function __git_prompt_color()
{
  if [ "$(git rev-parse --is-inside-git-dir 2>/dev/null)" = "true" ]; then
    echo black
  else
    if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
      echo red
    else
      echo green
    fi
  fi
}

function __git_prompt()
{
  local git_dir
  git_dir="$(git rev-parse --git-dir 2>/dev/null)" || return 0

  local prefix=""
  local postfix=""
  local upstream=""
  local stash=""
  local branch=""

  if [ -f "$git_dir/rebase-merge/interactive" ]; then
    postfix="|REBASE-i"
    branch="$(cat "$git_dir/rebase-merge/head-name")"
  elif [ -d "$git_dir/rebase-merge" ]; then
    postfix="|REBASE-m"
    branch="$(cat "$git_dir/rebase-merge/head-name")"
  else
    if [ -d "$git_dir/rebase-apply" ]; then
      if [ -f "$git_dir/rebase-apply/rebasing" ]; then
        postfix="|REBASE"
      elif [ -f "$git_dir/rebase-apply/applying" ]; then
        postfix="|AM"
      else
        postfix="|AM/REBASE"
      fi
    elif [ -f "$git_dir/MERGE_HEAD" ]; then
      postfix="|MERGING"
    elif [ -f "$git_dir/BISECT_LOG" ]; then
      postfix="|BISECTING"
    fi

    branch="$(git symbolic-ref HEAD 2>/dev/null)"
    test -n "$branch" || branch="## Detached head ##"
  fi

  if [ "$(git rev-parse --is-inside-git-dir 2>/dev/null)" = "true" ]; then
    if [ "$(git rev-parse --is-bare-repository 2>/dev/null)" = "true" ]; then
      prefix="BARE:"
    else
      branch="GIT_DIR!"
    fi
  else
    counts="$(git rev-list --count --left-right HEAD@{upstream}...HEAD 2>/dev/null)" && {
      case "$counts" in
      "0${__tab}0") # equal to upstream
        upstream=" =" ;;
      "0${__tab}"*) # ahead of upstream
        upstream=" $__arrow_up" ;;
      *"${__tab}0") # behind upstream
        upstream=" $__arrow_down" ;;
      *)            # diverged from upstream
        upstream=" $__arrow_up$__arrow_down" ;;
      esac
    }

    git rev-parse --verify refs/stash >/dev/null 2>&1 && stash=" [$]"
  fi

  printf " (%s$upstream)" "$prefix${branch##refs/heads/}$stash$postfix"
}

function __git_prompt_zsh()
{
  printf "%%{"
  case "$(__git_prompt_color)" in
  red) printf "$__red";;
  green) printf "$__green";;
  black) printf "$__black";;
  esac
  printf "%%}"
  __git_prompt
  printf "%%{${__black}%%}"
}


if [ -n "${BASH_VERSION-}" ]; then
  PS1='\[\033[34m\]\u@\h \[\033[33m\]\w\[`
    case "$(__git_prompt_color)" in
    red) printf '$__red';;
    green) printf '$__green';;
    black) printf '$__black';;
    esac`\]$(__git_prompt)\[\033[0m\] \$ '
elif [ -n "${ZSH_VERSION-}" ]; then
  setopt PROMPT_SUBST
  PS1=$'%{\033[34m%}%n@%m %{\033[33m%}%~$(__git_prompt_zsh)%{\033[0m%} %# '
fi
