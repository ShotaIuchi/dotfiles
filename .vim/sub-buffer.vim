"
" Init
"
let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable =1
let g:unite_source_file_mru_limit = 200

"
" Keybind
"
noremap <C-U><C-U> :<C-u>Unite buffer file_mru file bookmark<CR>
noremap <C-U><C-Y> :<C-u>Unite -buffer-name=register register<CR>

"
" Options
"
au FileType unite nnoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
au FileType unite nnoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>
au FileType unite call s:unite_my_settings()
    function! s:unite_my_settings()
endfunction

"
" Sarch options
"
call unite#custom#substitute('file', '\$\w\+', '\=eval(submatch(0))', 200)
call unite#custom#substitute('file', '^@@', '\=fnamemodify(expand("#"), ":p:h")."/"', 2)
call unite#custom#substitute('file', '^@', '\=getcwd()."/*"', 1)
call unite#custom#substitute('file', '^;r', '\=$VIMRUNTIME."/"')
call unite#custom#substitute('file', '^\~', escape($HOME, '\'), -2)
call unite#custom#substitute('file', '\\\@<! ', '\\ ', -20)
call unite#custom#substitute('file', '\\ \@!', '/', -30)
if has('win32') || has('win64')
    call unite#custom#substitute('file', '^;p', 'C:/Program Files/')
    call unite#custom#substitute('file', '^;v', '~/vimfiles/')
else
    call unite#custom#substitute('file', '^;v', '~/.vim/')
endif
