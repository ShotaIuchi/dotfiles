" local
if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif


" char
set ff=unix
set encoding=utf-8


" display
set laststatus=2
set t_Co=256
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
    syntax on
endif
set list
set listchars=tab:»-,trail:␠,extends:»,precedes:«,nbsp:%
set title
set number
set cursorline
set showmatch
set showcmd


" file
set autoread
set nobackup
set noswapfile


" edit
set backspace=indent,eol,start


" find/replace
set hlsearch
set incsearch
set ignorecase
set smartcase


" tab/indent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent
set smartindent


" sound
set visualbell t_vb=
set noerrorbells


" sub-settings
set runtimepath+=~/.vim/
runtime! package.vim
runtime! sub-*.vim
