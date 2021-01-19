#!/bin/sh

## ! ハードリンクにしたい場合は-sを削除

ln -s $(PWD)/.script.d ~/
ln -s $(PWD)/.gitconfig ~/
ln -s $(PWD)/.emacs.d ~/
ln -s $(PWD)/.vim ~/
ln -s $(PWD)/.vimrc ~/
ln -s $(PWD)/.uncrustify.cfg ~/
