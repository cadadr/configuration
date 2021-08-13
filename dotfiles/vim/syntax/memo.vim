" memo.vim --- syntax for bin/jot memos

if exists("b:current_syntax")
    finish
endif

if !exists('main_syntax')
    let main_syntax = 'memo'
endif

runtime! syntax/markdown.vim
unlet! b:current_syntax

syn region memoLink start=+\[\[+ end=+\]\]+ keepend
hi memoLink ctermfg=4

let b:current_syntax = "memo"
if main_syntax ==# 'memo'
    unlet main_syntax
endif

