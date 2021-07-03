" rc.vim --- vim configuration

" some sources I (ab)used way too much to note everywhere:
" - https://git.sr.ht/~admicos/dot/tree/master/nvim/init.vim

" UI/UX {{{
colorscheme gruvbox
set background=dark
set termguicolors

set nocompatible

set backspace=eol,start,indent	" backspace over newline too
set incsearch
set undofile		" persistent undo ...
set undodir=~/.vim/undo	" ... right here
set hidden		" don't unload buffers
set history=10000	" hard disks are bigger these days...
set mouse=a		" enable mouse support
set ignorecase		" case fold search
set nu			" show current line number
			" numbers

" Annotate whitespace
" ===================
" trail:    trailing whitespace
" tab:	    tab stop start and tab shift
" nbsp:	    non-breaking space
set list listchars=trail:-,tab:>-,nbsp:_

" GUI options
" ===========
" a:	write visual selection to X selection
" i:	set window icon
" e:	use GUI tabs for 'showtabline'
"
" toolbar, menubar, and scrollbars are turned off.
set guioptions=aie

" }}}


" basic text editing settings {{{

" Tab setup
" =========
" tabstop:	the size of hard tab (\t).  we keep it at 8 because
"		it's a sane default.
"
" softtabstop:	number of spaces to insert instead of tabs.
"
" shiftwidth:	number of spaces to indent with.
"
" expandtab:	insert spaces instead of tabs.  Ctrl-V<Tab> inserts
"		hard tab.
"
" smarttab:	in front insert 'shiftwidth blanks, 'ts' or 'sts'
"		elsewhere
set ts=8 softtabstop=4 shiftwidth=4 expandtab smarttab
set encoding=utf8
set textwidth=72	" 72 chars per line

" Text formatting
" ===============
" -r:	do not insert comment leader after <Enter>
" -o:	---------------- " --------------- 'o' or 'O'
" +n:	recognise numbered lists, viz. 'formatlistpat'
" +j:	remove comment leader when joining if applicable
" +1:	do not break line after one letter word if possible
" +p:	do not break line after period and single space
set formatoptions-=ro
set formatoptions+=nj1
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
set lbr			" word wrap
let &sbr = '\ '		" illustrate logical line breaks
set termguicolors	" use GUI colors in terminal
set hlsearch
set lazyredraw
set laststatus=2	" always show status bar
set scrolloff=2 sidescrolloff=2

" }}}

" custom commands {{{
" write file and sleep
command Wz norm :w<CR><C-z>

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
nmap <Leader>! 02wy$:!<C-r>"
nmap <Leader>w :Wz<CR>

" }}}

" plugins {{{
call plug#begin('~/.local/share/vim-plugins')
Plug 'tpope/vim-speeddating'
Plug 'SirVer/ultisnips'
Plug 'jceb/vim-orgmode'
Plug 'chrisbra/unicode.vim'
call plug#end()
" }}}

" postamle {{{
" This should be done after setting up runtimepath.  So guess it's best
" to put it last.
filetype plugin indent on
syntax on
" }}}

