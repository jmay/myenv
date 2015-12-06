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
# PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
# PATH=$PATH:/usr/local/rvm/bin # Add RVM to PATH for scripting

# 100316 JWM: http://rvm.beginrescueend.com/rvm/install/
RUNRVM=/usr/local/rvm/scripts/rvm
#RUNRVM=$HOME/.rvm/scripts/rvm
if [[ -s "$RUNRVM" ]]  ; then source "$RUNRVM" ; fi

### 14XXXX Heroku Toolbelt https://toolbelt.heroku.com/
export PATH="/usr/local/heroku/bin:$PATH"
