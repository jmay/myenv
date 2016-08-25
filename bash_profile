#!/bin/bash
# .bash_profile

. ~/.config/globals.sh

# Get the aliases and functions
if [ -f $BASH_ENV ]; then
  . $BASH_ENV
fi

umask 033 # prohibits write & execute permission for group & other when new files are created


# 080708 http://blog.macromates.com/2008/working-with-history-in-bash/
export HISTCONTROL=erasedups
export HISTSIZE=10000
shopt -s histappend

source ~/.config/my_shell_prompt.bash

# these seem to be obsolete methods for installing rvm
# PATH=$PATH:/usr/local/rvm/bin # Add RVM to PATH for scripting

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# 160824 experimented with asdf as alternative to rvm
# asdf is very new and has little penetration. Not trivial to migrate
# from rvm, probably easier to reinstall everything. In particular asdf
# doesn't automatically recognize GEM_HOME and GEM_PATH from rvm
# TODO: try this again in 2017
# . $HOME/.asdf/asdf.sh
# . $HOME/.asdf/completions/asdf.bash
