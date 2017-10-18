#!/bin/bash

#
# prompt
PS1="\[\e]0;\w\a\]\n\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[0m\]\n\$ "

#
# include
if [ -f ~/.bashrc.def ]; then
    source ~/.bashrc.def
fi
if [ -f ~/.bashrc.local ]; then
    source ~/.bashrc.local
fi

#
# alias
alias ls='ls -F --color=auto'
alias lc='ls -F --color=auto --show-control-char'
alias l='ls -lah'
alias ll='ls -lah'
alias lr='ls -lahrt'
alias v='vim'
alias vi='vim'
alias gg='git l'
alias gl='git l'

#
# function
function replace()
{
    find ${3} -type f | grep -v .svn | grep -v .git | xargs sed -i "s/${1}/${2}/g"
    echo "PATH: ${3}"
    echo "> \"${1}\" => \"${2}\""
}

