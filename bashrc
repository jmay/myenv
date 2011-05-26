# .bashrc

# User specific aliases and functions

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

export LS=/bin/ls

alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

alias c=clear
alias ls="$LS -v" # -v "force unedited printing" which gives proper UTF-8 display
alias l="$LS -v"
alias ll="$LS -alv" # -a include dot files
alias lt="$LS -altv" # -t order by modified timestamp, most recent first

alias cln="rm -i *~"

# usage: p
function p {
    if [ $# -gt 0 ]
    then
	ps aucx | head -1; ps aucx | grep -i $1
    else
	ps aucx
    fi
}

alias wget="curl -O"

alias mmd2="~/Library/Application\ Support/MultiMarkdown/bin/MultiMarkdown.pl"

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
