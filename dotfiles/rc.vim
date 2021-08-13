" rc.vim --- vim configuration

" some sources I (ab)used way too much to note everywhere:
" - https://git.sr.ht/~admicos/dot/tree/master/nvim/init.vim

    " KEEP THIS CONFIG BELOW 100SLoC and AVOID
    "             PLUGINS CHALLENGE!!!!
    " Count SLoC:
    " grep '^[^\s*"]' % | wc -l

" UI/UX {{{
colorscheme gruvbox
set background=dark
set termguicolors

set nocompatible

set backspace=eol,start,indent	" backspace over newline too
" incremental case fold search, in suit and tie yet ignored
set incsearch ignorecase smartcase
set undofile		" persistent undo ...
set undodir=~/.vim/undo	" ... right here
set hidden		" don't unload buffers
set history=10000	" hard disks are bigger these days...
set mouse=a		" enable mouse support
set nu			" show current line number

" Fix background colour problems
" ==============================
" Adapted from https://unix.stackexchange.com/a/615451
"
" vim hardcodes background color erase even if the terminfo file does
" not contain bce (not to mention that libvte based terminals
" incorrectly contain bce in their terminfo files). This causes
" incorrect background rendering when using a color theme with a
" background color.
let &t_ut=''

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

" Short message
" =============
" S:	do *not* show search count like [1/4]
set shortmess-=S

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


" Spell checking
" ==============
set spelllang=en,tr,it

" Spell checker options
" =====================
autocmd FileType text,markdown setlocal spell
autocmd FileType gitcommit setlocal spell

" }}}


" completion {{{
set path+=**		" recursive search
set wildmenu		" enhanced command-line completion

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


" keybindings {{{

" Give right pinky a break.
nmap <TAB> :

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
nmap <Leader>f {!}fmt<CR>
nmap <Leader>F 1G!Gfmt<CR>
nmap <Leader>s {!}sort<CR>
nmap <Leader>S 1G!Gsort<CR>
nmap <Leader>! 02wy$:!<C-r>"
nmap <Leader>w :w<CR><C-z>

" bin/jot
au FileType memo nnoremap <buffer> <Leader>j :call JotFollow()<CR>

" Utilities {{{

" Create parent directories if needed
" ===================================
" From: https://github.com/duggiefresh/vim-easydir
function! GkMkdirP()
    let l:dir = expand('<afile>:p:h')
    if l:dir !~# '^\w\+:' && !isdirectory(l:dir)
	call mkdir(l:dir, 'p')
	echomsg 'Created directory: ' . l:dir
    endif
endfunction

au BufWritePre,FileWritePre * call GkMkdirP()


" Transparent editing of gpg encrypted files
" ==========================================
" By Wouter Hanegraaff <wouter@blub.net>
" Thank you, @dredmorbius!
augroup encrypted
    au!

    " First make sure nothing is written to ~/.viminfo while editing
    " an encrypted file.
    autocmd BufReadPre,FileReadPre      *.asc set viminfo=
    " We don't want a swap file, as it writes unencrypted data to disk
    autocmd BufReadPre,FileReadPre      *.asc set noswapfile
    " Switch to binary mode to read the encrypted file
    autocmd BufReadPre,FileReadPre      *.asc set bin
    autocmd BufReadPre,FileReadPre      *.asc let ch_save = &ch|set ch=2
    autocmd BufReadPost,FileReadPost    *.asc '[,']!gpg --decrypt 2> /dev/null
    " Switch to normal mode for editing
    autocmd BufReadPost,FileReadPost    *.asc set nobin
    autocmd BufReadPost,FileReadPost    *.asc let &ch = ch_save|unlet ch_save
    autocmd BufReadPost,FileReadPost    *.asc execute ":doautocmd BufReadPost " . expand("%:r")

    " Convert all text to encrypted text before writing
    autocmd BufWritePre,FileWritePre    *.asc '[,']!gpg --default-recipient-self -ae 2>/dev/null
    " Undo the encryption so we are back in the normal text, directly
    " after the file has been written.
    autocmd BufWritePost,FileWritePost  *.asc u
augroup END

" }}}


" postamble {{{

" File associations
au BufReadPost *.crontab set ft=crontab
au BufReadPost *.memo set ft=memo

" This should be done after setting up runtimepath.  So guess it's best
" to put it last.
filetype plugin indent on
syntax on
" }}}

