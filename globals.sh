# my global variable

export BASH_ENV=$HOME/.bashrc

export PERL5LIB=$HOME/.perl/lib/perl5/site_perl:$HOME/.perl/lib/perl5/5.8.9:$PERL5LIB

# for rvm
export RVM_HOME=$HOME/.rvm

# EC2 commands
export EC2_HOME=/Users/jmay/Projects/src/ec2-api-tools-1.3-62308
export EC2_PRIVATE_KEY=/Users/jmay/.ec2/pk-IYVNNUFLAS56IFQT6XNCH3PN6KODDTMJ.pem
export EC2_CERT=/Users/jmay/.ec2/cert-IYVNNUFLAS56IFQT6XNCH3PN6KODDTMJ.pem

export JAVA_HOME=/Library/Java/Home

# my paths
export SYSTEM_PATH=$PATH
export MY_PATH=$HOME/bin:$RVM_HOME/bin:/Users/jmay/.perl/bin:$GEM_HOME/bin
export LOCALS_PATH=/usr/local/mysql/bin:/Developer/usr/bin:/opt/local/sbin:/usr/local/bin:$EC2_HOME/bin

export PATH=$MY_PATH:$LOCALS_PATH:$SYSTEM_PATH

export LD_LIBRARY_PATH=/opt/local/lib:/usr/local/lib

# http://macromates.com/textmate/manual/bundles#getting_more_bundles
# says I need this for svn to handle UTF-8 correctly
export LC_CTYPE=en_US.UTF-8

# use TextMate for svn commits etc.
export EDITOR="mate -w"

# 080708 http://blog.macromates.com/2008/working-with-history-in-bash/
export HISTCONTROL=erasedups # suppresses recording any duplicate entries in history
export HISTSIZE=10000
shopt -s histappend # append to shell history file (.bash_history), instead of overwriting

export RUBYOPT=rubygems

# where nvALT & nvwatch.rb looks for files
export NOTESDIR=$HOME/Dropbox/Documents/Notes/

# for ec2-api-tools
export JAVA_HOME="$(/usr/libexec/java_home)"
export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.ec2/pk-*.pem)"
export EC2_CERT="$(/bin/ls $HOME/.ec2/cert-*.pem)"
export EC2_HOME="/usr/local/Cellar/ec2-api-tools/1.5.2.3/jars"
