" memo.vim --- bin/jot memo functions

" Resolve [[Jot_Links]]
function JotResolve(name)
    let r = system('jot -R ' . a:name)
    execute('e ' . r)
endfunction

" Follow [[Jot_Links]]
function JotFollow()
    " XXX: maybe use normal! here?
    silent! normal "jyJ
    call JotResolve(getreg('j'))
endfunction

