# .bashrc 

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

. ~/env/aliases.bash
. ~/env/functions.bash

# usage: p
function p {
  if [ $# -gt 0 ]
  then
  	ps aucx | head -1; ps aucx | grep -i $1
  else
  	ps aucx
  fi
}

# http://effectif.com/mac-os-x/textmate/opening-ruby-gems-in-textmate
_mategem()
{
    local curw
    COMPREPLY=()
    curw=${COMP_WORDS[COMP_CWORD]}
    local gems="$(gem environment gemdir)/gems"
    COMPREPLY=($(compgen -W '$(ls $gems)' -- $curw));
    return 0
}
complete -F _mategem -o dirnames mategem
