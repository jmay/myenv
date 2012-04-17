# My standard bash prompt
# Jason W. May - jmay@pobox.com
# January 2012
#
# include hostname, current git repo status

# colors
# https://wiki.archlinux.org/index.php/Color_Bash_Prompt
color_red=`echo -e "\033[0;31m"`
color_green=`echo -e "\033[0;32m"`
color_reset=`echo -e "\033[0m"`

function status_sigil {
  if [[ $1 -eq 0 ]]
  then
    sigil=☺
    sigilcolor=$color_green
  else
    sigil=☹
    sigilcolor=$color_red
  fi
}

# 071214 JWM: reset terminal window title every time shell prompt is displayed
# http://www.macosxhints.com/article.php?story=20031015173932306&query=bookmarks
# TODO: only change prompt when you change directory?
# executes this immediately before displaying $PS1
export PROMPT_COMMAND='status_sigil $?;\
  echo -ne "\033]0;${HOSTNAME%%.*}: ";\
  basename "${PWD}";\
  echo -ne "\007"'

# 090327 http://www.hasmanybeers.com/2009/03/custom-bash-prompt-for-git-branches.html
# adds the current git branch to the command prompt
source ~/env/git_completion.sh

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

# autocompletion for ssh
_comp_ssh ()
{
  cur=${COMP_WORDS[COMP_CWORD]};
  hosts=`/bin/cat ~/.ssh/config | grep 'Host ' | sed 's/.* //'`
  COMPREPLY=($(compgen -W '$hosts --all --schema' -- $cur))
}

complete -F _comp_ssh ssh

# prompt should look like:
# smiley-space-hostname-colon-basename-{[-branch-dirty-]} where the branch/dirty bits are only shown for git directories
# \[ and \] delimiters are needed around non-printing sections of the prompt, otherwise readline gets confused

export PS1='\[${sigilcolor}\]${sigil}\[${color_reset}\] \h:\W$(__git_ps1 "[\[${color_green}\]%s\[${color_red}\]$(parse_git_dirty)\[${color_reset}\]]")$ '
