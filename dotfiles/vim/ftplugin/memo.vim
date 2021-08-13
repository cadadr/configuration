" memo.vim --- bin/jot memo filetype plugin

if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/markdown.vim

" Jot link text object
vnoremap J :<C-U>silent! normal! vi[<ESC><ESC>vi[
omap     J :normal vJ<CR>

