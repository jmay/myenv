# My standard bash prompt
# Jason W. May - jmay@pobox.com
# January 2012
#
# include hostname, current git repo status

# 071214 JWM: reset terminal window title every time shell prompt is displayed
# http://www.macosxhints.com/article.php?story=20031015173932306&query=bookmarks
# TODO: only change prompt when you change directory?
# executes this immediately before displaying $PS1
export PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME%%.*}: "; basename "${PWD}"; echo -ne "\007"'

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
