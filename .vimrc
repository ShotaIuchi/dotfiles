" char
set ff=unix
set encoding=utf-8


" display
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
    syntax on
endif
set list
set listchars=tab:»-,trail:␠,extends:»,precedes:«,nbsp:%
set title
set number
set cursorline
set showmatch


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

