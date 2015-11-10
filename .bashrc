#
# include
if [ -f .bashrc.def ]; then
    source .bashrc.def
fi
if [ -f .bashrc.local ]; then
    source .bashrc.local
fi

#
# alias
alias l='ls -lah'
alias ll='ls -lah'
alias lr='ls -lahrt'
alias v='vim'
alias vi='vim'
alias gg='git l'
alias gl='git l'

