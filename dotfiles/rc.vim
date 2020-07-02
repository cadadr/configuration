" rc.vim --- vim configuration

" I don't use vim much, but it can be useful to maintain a minimal vimrc, so
" here is this file.  This one is meant to be symlinked from $HOME/.vimrc.

" turn off compatibility and enable modern features.
set nocompatible
" syntax enable
filetype plugin on

" basic text editing settings
set textwidth=72	" 72 chars per line
set formatoptions-=tc	" don't auto hardwrap tho
set noexpandtab

" completion
set path+=**		" recursive search
set wildmenu		" enhanced command-line completion

" tags
" generate tags:
command! MkTags !/usr/bin/ctags -R .

" bindings
" make Y behave like C, D, etc., i.e. operate from point to eol
nmap Y y$

" visuals
set lbr			" word wrap
let &sbr = '\ '		" illustrate logical line breaks
set relativenumber      " line numbers relative to current line
set number		" but current line number for current line
" set cursorline          " highlight current line

" keybindings
nmap <Leader>n :set nu<CR>
nmap <Leader>r :source ~/.vimrc<CR>
nmap <Leader>l :set list!<CR>
nmap <Leader>f {V}:'<,'>!fmt<CR>
nmap <Leader>F :0,$!fmt<CR>
" Copy to clipboard as markdown code snippet
noremap <Leader>M :s/^/    /<CR>gv"+yu
nmap <Leader>z :w<CR>

