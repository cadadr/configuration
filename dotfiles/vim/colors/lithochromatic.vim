" LithoChromatic - Color Theme for Vim
" Copyright (C) 2013 Göktuğ Kayaalp <goktug.kayaalp@gmail.com>
"
" LithoChromatic is licensed under the terms of MIT License; for license text
" see the LICENSE file at the project root, or visit this URL:
"
"   http://opensource.org/licenses/MIT

" Some boilerplate code here.
set background="light"
highlight clear
if exists("syntax_on")
	syntax reset
endif

let g:colors_name = "lithochromatic"

highlight Include ctermfg=22 guifg=#005f00
highlight Type  ctermfg=127 guifg=#af00af
highlight Statement ctermfg=127 guifg=#af00af
highlight StorageClass ctermfg=127 guifg=#af00af
highlight Conditional ctermfg=127 guifg=#af00af
highlight Constant ctermfg=160 guifg=#d70000
highlight String ctermfg=22 cterm=NONE guifg=#005f00 gui=NONE
highlight Special ctermfg=17 guifg=#00005f
highlight Comment ctermfg=18 guifg=#000087 gui=italic
highlight Number ctermfg=61 guifg=#5f5faf cterm=bold
highlight Identifier ctermfg=127 guifg=#af00af
highlight Ignore ctermbg=255 ctermfg=16 guifg=#000000 guibg=#eeeeee
highlight link  Boolean  Type
highlight link Function Type
highlight link Keyword  Type
highlight link Structure StorageClass
highlight link Define  Include
highlight link PreCondit  Define
highlight link Label   Statement
highlight link Repeat   Conditional
highlight link Operator  Conditional

highlight Normal  ctermbg=254 ctermfg=16 guifg=#000000 guibg=#eeeeee
highlight Title ctermbg=255 ctermfg=16 guifg=#000000 guibg=#eeeeee cterm=underline gui=underline
highlight ErrorMsg  ctermbg=52 ctermfg=255 guifg=#eeeeee guibg=#5f0000
highlight LineNr  ctermfg=238 ctermbg=144 guifg=#444444 guibg=#afaf87
highlight Pmenu  ctermbg=144 ctermfg=0 guifg=#000000 guibg=#afaf87
highlight PmenuSel  ctermbg=52 ctermfg=144 guifg=#afaf87 guibg=#5f0000
highlight PmenuSbar  ctermfg=52 ctermbg=0 guifg=#5f0000 guibg=#000000
highlight PmenuThumb  ctermfg=52 guifg=#5f0000
highlight ModeMsg  ctermfg=22 guifg=#005f00
highlight Search  ctermbg=NONE ctermfg=NONE guifg=NONE guibg=NONE cterm=underline, gui=underline,
highlight Visual  ctermbg=191 ctermfg=0 guifg=#000000 guibg=#d7ff5f
highlight ColorColumn  ctermbg=250 ctermfg=NONE guibg=#bcbcbc
highlight MatchParen  cterm=underline, ctermbg=NONE ctermfg=16 guifg=#000000 guibg=NONE gui=underline,
highlight NonText ctermbg=NONE guibg=NONE ctermfg=203 guifg=#ff5f5f
highlight SpecialKey ctermbg=NONE ctermfg=203 guibg=NONE guifg=#ff5f5f
highlight Question ctermfg=22 guifg=#005f00
highlight MoreMsg ctermfg=22 guifg=#005f00
highlight Directory ctermfg=22 guifg=#005f00
highlight WarningMsg ctermbg=52 ctermfg=255 guifg=#eeeeee guibg=#5f0000
highlight CursorLineNr ctermfg=238 ctermbg=144 guifg=#444444 guibg=#afaf87 cterm=underline gui=underline
highlight Folded ctermbg=191 ctermfg=0 guifg=#000000 guibg=#d7ff5f
highlight FoldColumn ctermbg=191 ctermfg=0 guifg=#000000 guibg=#d7ff5f
highlight ExtraWhitespace ctermbg=255 guibg=#eeeeee ctermfg=250 guifg=#bcbcbc
highlight DiffAdd ctermbg=NONE ctermfg=34 guifg=#00af00 guibg=NONE
highlight DiffChange ctermbg=NONE ctermfg=22 guifg=#005f00 guibg=NONE
highlight DiffText ctermbg=NONE ctermfg=160 guifg=#d70000 guibg=NONE cterm=underline, gui=underline
highlight DiffDelete ctermbg=NONE ctermfg=160 guifg=#d70000 guibg=NONE
highlight CursorColumn ctermbg=231 ctermfg=NONE guibg=#ffffff cterm=NONE guifg=NONE gui=NONE
highlight CursorLine ctermbg=229 ctermfg=NONE guibg=#ffffaf cterm=NONE guifg=NONE gui=NONE
highlight TabLine cterm=underline, gui=underline,
highlight VertSplit ctermfg=16 guifg=#000000 ctermbg=16 guibg=#000000
highlight link Conceal ExtraWhitespace

"highlight DefinedName
"highlight EnumerationValue
"highlight VisualNOS
"highlight Member
"highlight GlobalVariable
"highlight StatusLine

" Markdown
highlight markdownH1 ctermfg=NONE guifg=NONE cterm=bold gui=bold
highlight markdownH2 ctermfg=NONE guifg=NONE cterm=bold gui=bold
highlight markdownH3 ctermfg=NONE guifg=NONE cterm=bold gui=bold
highlight markdownH4 ctermfg=NONE guifg=NONE cterm=bold gui=bold
highlight markdownH5 ctermfg=NONE guifg=NONE cterm=bold gui=bold
highlight markdownH6 ctermfg=NONE guifg=NONE cterm=bold gui=bold
highlight markdownH7 ctermfg=NONE guifg=NONE cterm=bold gui=bold
highlight markdownLinkText ctermfg=17 cterm=bold guifg=#00005f gui=bold
highlight markdownListMarker ctermfg=160 guifg=#d70000
highlight link markdownHeadingRule markdownH1
highlight link markdownItalic String
highlight link markdown String
highlight link markdownIdDeclaration markdownLinkText
highlight link markdownUrl markdownLinkText

" Python
highlight link pythonFunction Normal
highlight link pythonExceptions Normal

" VimL
highlight VimHiAttrib ctermfg=232 guifg=#080808
highlight link VimSet Type
highlight link VimOption Normal
highlight link helpHyperTextJump markdownUrl
highlight link vimVar Normal
highlight link vimHiGroup Normal

" Perl
highlight link perlVarPlain Normal
highlight link perlVarPlain2 Normal
highlight link perlSubName Normal

" Ruby
highlight link rubyFunction Normal
highlight link rubyDefine Keyword

" JavaScript
highlight link javaScriptBraces Normal

" Css
highlight link cssBraces Normal
highlight link cssTagName Normal
highlight link cssClassName String
highlight link cssIdentifier String

" Spelling
highlight SpellBad cterm=underline ctermfg=red ctermbg=NONE gui=underline guifg=red guibg=NONE
highlight SpellCap cterm=underline ctermfg=blue ctermbg=NONE gui=underline guifg=blue guibg=NONE
highlight SpellLocal cterm=underline ctermfg=green ctermbg=NONE gui=underline guifg=green guibg=NONE
