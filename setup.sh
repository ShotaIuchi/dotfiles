#!/bin/sh
#
# variable
PATH_ROOT=~
PATH_DOTFILES=~/dotfiles

#
# function
function createDir()
{
    dst=${PATH_DOTFILES}/${1}
    if [ -d ${dst} ]; then
        echo "Allready directory '${dst}'"
    else
        mkdir -p ${dst}
    fi
}


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
initLink '.vim'
initLink '.vimrc'


#
# thard party
function cloneGit()
{
    src=${2}
    dst=${PATH_DOTFILES}/${1}
    if [ -d ${dst} ]; then
        echo "Allready clone '${src}'"
    else
        git clone ${src} ${dst}
    fi
}
createDir '.vim/bundle'
cloneGit '.vim/bundle/neobundle.vim' 'https://github.com/Shougo/neobundle.vim'
