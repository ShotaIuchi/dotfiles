#!/bin/sh
#
# variable
PATH_ROOT=~
PATH_DOTFILES=~/dotfiles

#
# link
function initLink()
{
    src=${PATH_DOTFILES}/${1}
    dst=${PATH_ROOT}/${1}
    if [ -f ${dst} ] || [ -d ${dst} ]; then
        if [ ! -h ${dst} ]; then
            mv ${dst} ${dst}.local
            ln -s ${src} ${dst}
        fi
    else
        ln -s ${src} ${dst}
    fi
}
initLink '.bashrc'
initLink '.emacs.d'
initLink '.gitconfig'
initLink '.vimrc'
