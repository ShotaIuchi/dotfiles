#!/bin/sh
source ./.script.d/SetupLink.sh

TARGET_PATH=~
THIS_PATH=~/dotfiles

SetupLink ${TARGET_PATH} ${THIS_PATH} .bashrc
SetupLink ${TARGET_PATH} ${THIS_PATH} .script.d
SetupLink ${TARGET_PATH} ${THIS_PATH} .gitconfig
setupLink ${TARGET_PATH} ${THIS_PATH} .emacs.d
SetupLink ${TARGET_PATH} ${THIS_PATH} .vim
SetupLink ${TARGET_PATH} ${THIS_PATH} .vimrc
SetupLink ${TARGET_PATH} ${THIS_PATH} .uncrustify.cfg
