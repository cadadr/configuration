" rc.vim --- vim configuration

" I don't use vim much, but it can be useful to maintain a minimal vimrc, so
" here is this file.  This one is meant to be symlinked from $HOME/.vimrc.

" some sources I (ab)used way too much to note everywhere:
" - https://git.sr.ht/~admicos/dot/tree/master/nvim/init.vim

" UI/UX {{{
set nocompatible
filetype plugin indent on
set encoding=utf8
set backspace=eol,start,indent	" backspace over newline too
set incsearch
set undofile		" persistent undo ...
set undodir=~/.vim/undo	" ... right here
set hidden		" don't unload buffers
set history=10000	" hard disks are bigger these days...
set mouse=a		" enable mouse support
set ignorecase		" case fold search

" guioptions:
" a: write visual selection to X selection
" i: set window icon
" e: use GUI tabs for 'showtabline'
"
" toolbar, menubar, and scrollbars are turned off.
set guioptions=aie

" }}}


" basic text editing settings {{{
set textwidth=72	" 72 chars per line
set formatoptions-=tc	" don't auto hardwrap tho
set noexpandtab
set autoindent
set foldmethod=marker	" manually inserted markers
set foldlevelstart=99	" show content at startup

" }}}


" completion {{{
set path+=**		" recursive search
set wildmenu		" enhanced command-line completion

" }}}


" tags {{{
" generate tags:
command! MkTags !/usr/bin/ctags -R .

" }}}


" visuals {{{
colorscheme zenburn
set lbr			" word wrap
let &sbr = '\ '		" illustrate logical line breaks
set termguicolors	" use GUI colors in terminal
set hlsearch
set nottyfast		" for ssh
set lazyredraw
set laststatus=2	" always show status bar
set scrolloff=2 sidescrolloff=2

" }}}


" keybindings {{{

" make Y behave like C, D, etc., i.e. operate from point to eol
nmap Y y$

" Window movement binds
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-h> <C-W>h
noremap <C-l> <C-W>l

" Buffer movement
noremap <C-n> :bnext<cr>
noremap <C-p> :bprevious<cr>

nmap , <leader>
nmap <Leader>n :set nu<CR>
nmap <Leader>r :source ~/.vimrc<CR>
nmap <Leader>l :set list!<CR>
nmap <Leader>f {V}:'<,'>!fmt<CR>
nmap <Leader>F :0,$!fmt<CR>
" Copy to clipboard as markdown code snippet
nmap <Leader>M :s/^/    /<CR>gv"+yu
nmap <Leader>z :w<CR>
nmap <Leader>p "*p
nmap <Leader>P "*P
nmap <Leader>y "*y
nmap <Leader>Y "*Y

" }}}

