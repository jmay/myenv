# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# https://github.com/mrzool/bash-sensible
if [ -f ~/.config/sensible.bash ]; then
  if [[ $- =~ i ]]
  then
    # only run this for interactive shells
    source ~/.config/sensible.bash
  fi
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
