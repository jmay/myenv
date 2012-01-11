# My standard shell aliases
# Jason W. May - jmay@pobox.com
# January 2012
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
