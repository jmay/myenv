#!/bin/bash
# my global variable

export BASH_ENV=$HOME/.bashrc

# # perl
# export PERL5LIB=$HOME/.perl/lib/perl5/site_perl:$HOME/.perl/lib/perl5/5.8.9:$PERL5LIB

# for rvm & ruby
# export RVM_HOME=$HOME/.rvm
# export RUBYOPT=rubygems

# node
export NODE_PATH=/usr/local/lib/node_modules

# my paths
export SYSTEM_PATH=$PATH
export MY_PATH=$HOME/bin #:/Users/jmay/.perl/bin:$GEM_HOME/bin
export LOCALS_PATH=/usr/local/bin:/usr/local/sbin
export PG_PATH=`echo /Applications/Postgres.app/Contents/Versions/9.4/bin`

export PATH=$MY_PATH:$LOCALS_PATH:$PG_PATH:$SYSTEM_PATH

# export LD_LIBRARY_PATH=/opt/local/lib:/usr/local/lib

# http://macromates.com/textmate/manual/bundles#getting_more_bundles
# says I need this for svn to handle UTF-8 correctly
export LC_CTYPE=en_US.UTF-8

#
export EDITOR=$HOME/bin/e

# 080708 http://blog.macromates.com/2008/working-with-history-in-bash/
export HISTCONTROL=erasedups # suppresses recording any duplicate entries in history
export HISTSIZE=10000
shopt -s histappend # append to shell history file (.bash_history), instead of overwriting


# where nvALT & nvwatch.rb looks for files
export NOTESDIR=$HOME/Dropbox/Documents/Notes/

# Java
# export JAVA_HOME=/Library/Java/Home
if [ -f "/usr/libexec/java_home" ]; then
  export JAVA_HOME="$(/usr/libexec/java_home)"
else
  javabin=`which java`
  export JAVA_HOME=`dirname $javabin`
fi

if [ -d $HOME/.ec2 ]; then
	export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
	export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem)"
	export EC2_HOME="/usr/local/Cellar/ec2-api-tools/1.5.2.3/jars"
fi

# suffixes to ignore when doing filename completion
export FIGNORE=".o:~:Application Scripts"
