" -----------------------------------------------------------------------------
" File: gk.vim
" Description: Autoloaded functions for my vimrc
" Author: Göktuğ Kayaalp <self at gkayaalp dot com>
" Source: https://github.com/cadadr/configuration
" Last Modified: 09 Apr 2014
" -----------------------------------------------------------------------------

" Create parent directories of buffer file if needed
" ==================================================
" From: https://github.com/duggiefresh/vim-easydir
function! gk#mkdir_p()
    let l:dir = expand('<afile>:p:h')
    if l:dir !~# '^\w\+:' && !isdirectory(l:dir)
	call mkdir(l:dir, 'p')
	echomsg 'Created directory: ' . l:dir
    endif
endfunction

" Ask to lock RCS controlled file
" ===============================
" Bind this to BufReadPost.
function! gk#rcs_co_l()
    let l:file = expand('<afile>')
    if !filewritable(l:file) || executable('rcsdiff') || executable('co')
	" rcsdiff will return non-zero if file is not RCS-controlled.
	let l:out1 = system("rcsdiff -q " . expand('<afile>::S'))
	if v:shell_error == 0
	    echomsg "found rcs file: " . l:file
	    let l:choice = confirm(
		    \ l:file . " is an RCS controlled file and is unlocked.\n" .
		    \ "Would you like me to check it out and unlock it for you?",
		    \ "&Yup\n&Nope")
	    if l:choice == 1
		let l:out2 = system("co -l " . expand('<afile>::S'))
		if v:shell_error != 0
		    echoerr "RCS checkout error: " . l:out2
		endif
	    endif
	endif
    endif
    silent edit!
    redraw!
endfunction
