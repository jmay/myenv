# My standard shell aliases and bindings
# Jason W. May - jmay@pobox.com
# last updated: 4 Sep 2014
#
# aliases are not inherited by subshells, so these should be loaded by .bashrc

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias cln="rm -i *~" # clean up emacs junk

alias c=clear

LS=/bin/ls
# -v "force unedited printing" which gives proper UTF-8 display (otherwise Unicode chars will appear as ?)
alias ls="$LS -v"
alias ll="$LS -alv" # -a include dot files
alias lt="$LS -altv" # -t order by modified timestamp, most recent first

if [ -z `which wget 2>/dev/null` ]; then
  # macosx does not have wget by default
  alias wget="curl -O"
fi

if [ -d ~/Library ]; then
  # convert Markdown to HTML
  alias mmd="~/Library/Application\ Support/MultiMarkdown/bin/MultiMarkdown.pl"
fi

alias s3j="s3cmd -c ~/.s3cfg-jmay"
alias s3v="s3cmd -c ~/.s3cfg-veriphyr"

# if `hub` (http://defunkt.io/hub/) is installed, then alias it to git
if [ `which hub 2>/dev/null` ]; then
  alias git=hub
fi

# from http://furbo.org/2014/09/03/the-terminal/
# so that up-arrow and down-arrow do history search using a prefix
# bind '"\e[A":history-search-backward'
# bind '"\e[B":history-search-forward'

alias brake="bundle exec rake"
alias be="bundle exec"
