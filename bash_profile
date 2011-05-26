# .bash_profile

BASH_ENV=$HOME/.bashrc
export BASH_ENV

# Get the aliases and functions
if [ -f $BASH_ENV ]; then
	. $BASH_ENV
fi

umask 033

# this is for fink

# . /sw/bin/init.sh

# my PERL5LIB has
#  ~/.perl/etc. for modules I've installed privately
#  paths set by the fink setup above

export PERL5LIB=$HOME/.perl/lib/perl5/site_perl:$HOME/.perl/lib/perl5/5.8.9:$PERL5LIB

# for rvm
export RVM_HOME=$HOME/.rvm

# moved this entirely to rvm
# for gems
# export GEM_HOME=$HOME/.gems
# export GEM_PATH=$HOME/.gems:/opt/local/lib/ruby/gems/1.8

# EC2 commands
export EC2_HOME=/Users/jmay/Projects/src/ec2-api-tools-1.3-62308
export EC2_PRIVATE_KEY=/Users/jmay/.ec2/pk-IYVNNUFLAS56IFQT6XNCH3PN6KODDTMJ.pem
export EC2_CERT=/Users/jmay/.ec2/cert-IYVNNUFLAS56IFQT6XNCH3PN6KODDTMJ.pem
export JAVA_HOME=/Library/Java/Home

# my paths
# DarwinPorts (http://www.macosforge.org/) puts things in /opt/local
export SYSTEM_PATH=$PATH
export MY_PATH=$HOME/bin:$RVM_HOME/bin:/Users/jmay/.perl/bin:$GEM_HOME/bin
export LOCALS_PATH=/usr/local/mysql/bin:/Developer/usr/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:$EC2_HOME/bin
# export PATH=$HOME/bin:$GEM_HOME/bin:/home/jmay/perl/bin:/usr/local/mysql/bin:/opt/local/bin:/usr/local/bin:$PATH

export LD_LIBRARY_PATH=/opt/local/lib:/usr/local/lib

# http://macromates.com/textmate/manual/bundles#getting_more_bundles
# says I need this for svn to handle UTF-8 correctly
export LC_CTYPE=en_US.UTF-8

# use TextMate for svn commits etc.
export EDITOR="mate -w"

export RUBYOPT=rubygems
export PATH=$MY_PATH:$LOCALS_PATH:$SYSTEM_PATH

# 071214 JWM: reset terminal window title every time shell prompt is displayed
# http://www.macosxhints.com/article.php?story=20031015173932306&query=bookmarks
# TODO: only change prompt when you change directory?
# executes this immediately before displaying $PS1
export PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME%%.*}: "; basename "${PWD}"; echo -ne "\007"'

# export MANPATH=/usr/local/git/man:$MANPATH

# 080708 http://blog.macromates.com/2008/working-with-history-in-bash/
export HISTCONTROL=erasedups
export HISTSIZE=10000
shopt -s histappend


# 090327 http://www.hasmanybeers.com/2009/03/custom-bash-prompt-for-git-branches.html
# adds the current git branch to the command prompt
source ~/env/git_completion.sh

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

export PS1='\h:\W$(__git_ps1 "[\[\e[0;32m\]%s\[\e[0m\]\[\e[0;33m\]$(parse_git_dirty)\[\e[0m\]]")$ '

##
# Your previous /Users/jmay/.bash_profile file was backed up as /Users/jmay/.bash_profile.macports-saved_2009-11-04_at_13:15:16
##

# 100316 JWM: http://rvm.beginrescueend.com/rvm/install/
if [[ -s "$HOME/.rvm/scripts/rvm" ]]  ; then source "$HOME/.rvm/scripts/rvm" ; fi
