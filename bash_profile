# .bash_profile

. ~/env/globals.sh

# Get the aliases and functions
if [ -f $BASH_ENV ]; then
	. $BASH_ENV
fi

umask 033 # prohibits write & execute permission for group & other when new files are created


# 080708 http://blog.macromates.com/2008/working-with-history-in-bash/
export HISTCONTROL=erasedups
export HISTSIZE=10000
shopt -s histappend

source ~/env/my_shell_prompt.bash

# 100316 JWM: http://rvm.beginrescueend.com/rvm/install/
if [[ -s "$HOME/.rvm/scripts/rvm" ]]  ; then source "$HOME/.rvm/scripts/rvm" ; fi
