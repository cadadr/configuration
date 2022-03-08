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

" Base URL for Zotero's API
let g:gk_zotero_base_url = 'http://127.0.0.1:23119'

" Insert citation using Zotero's «Cite as you Write» API
" ======================================================
" Adapted from: https://retorque.re/zotero-better-bibtex/citing/cayw/
function! gk#zotero_cite()
    " pick a format based on the filetype (customize at will)
    let format = &filetype =~ '.*tex' ? 'citep' : 'pandoc'
    let api_call = g:gk_zotero_base_url . '/better-bibtex/cayw?format=' . format . '&brackets=1'
    let ref = system('curl -s '.shellescape(api_call))
    return ref
endfunction

" Set selected item in Zotero from citation under point
function! gk#zotero_select()
    let dq = getreg('"')
    " TODO: this is simplistic, even tho it'd work for my keys most of
    " the time.
    normal yiw
    let key = getreg('"')
    call setreg('"', dq)
    let base_url = g:gk_zotero_base_url . '/zotxt/select?betterbibtexkey='
    let api_call = base_url . key
    let ref = system('curl -s ' . shellescape(api_call))
    echomsg ref
    return ref
endfunction

" Get information about the item under point in Zotero
" ====================================================
" Selects the item in Zotero first.
function! gk#zotero_info()
    call gk#zotero_select()
    let api_call = g:gk_zotero_base_url . '/zotxt/items?selected=selected&format=bibliography'
    let filter = '| jq .[0].text | tr -d "\\n"'
    let ref = system('curl -s ' . shellescape(api_call) . filter)
    echomsg ref
    return ref
endfunction

