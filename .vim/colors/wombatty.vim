" Vim color file
" Original Maintainer:  Lars H. Nielsen (dengmao@gmail.com)
" Modified By: Conner McDaniel (connermcd@gmail.com)
" Last Change: 2012-09-29
"
" Modified version of wombat for 256-color terminals by
"   David Liang (bmdavll@gmail.com)
" based on version by
"   Danila Bespalov (danila.bespalov@gmail.com)

set background=dark

if version > 580
	hi clear
	if exists("syntax_on")
		syntax reset
	endif
endif

let colors_name = "wombatty"


" Vim >= 7.0 specific colors
if version >= 700
hi CursorLine					ctermbg=236		cterm=none						guibg=#32322f
hi CursorLineNr	ctermfg=245		ctermbg=236		cterm=none		guifg=#8a8a8a	guibg=#32322f
hi MatchParen	ctermfg=228		ctermbg=101		cterm=bold		guifg=#eae788	guibg=#857b6f	gui=bold
hi Pmenu		ctermfg=230		ctermbg=238						guifg=#ffffd7	guibg=#444444
" hi PmenuSel		ctermfg=232		ctermbg=192						guifg=#080808	guibg=#cae982
hi PmenuSel		ctermfg=0		ctermbg=11						guifg=#080808	guibg=#cae982
endif

" General colors
hi Normal		ctermfg=252						cterm=none		guifg=#e3e0d7	guibg=#000000	gui=none
hi Cursor		ctermfg=000		ctermbg=228		cterm=none		guifg=#000000	guibg=#eae788	gui=none
hi Visual		ctermfg=251		ctermbg=239		cterm=none		guifg=#c3c6ca	guibg=#554d4b	gui=none
hi VisualNOS	ctermfg=251		ctermbg=236		cterm=none		guifg=#c3c6ca	guibg=#303030	gui=none
hi Search		ctermfg=177		ctermbg=241		cterm=none		guifg=#d787ff	guibg=#636066	gui=none
hi Folded		ctermfg=241		ctermbg=233		cterm=none		guifg=#857b6f	guibg=#000000	gui=none
hi Title		ctermfg=024						cterm=bold		guifg=#1077A5					gui=bold
hi StatusLine	ctermfg=230		ctermbg=238		cterm=none		guifg=#ffffd7	guibg=#444444	gui=italic
hi VertSplit	ctermfg=238		ctermbg=238		cterm=none		guifg=#444444	guibg=#444444	gui=none
hi StatusLineNC	ctermfg=241		ctermbg=238		cterm=none		guifg=#857b6f	guibg=#444444	gui=none
hi LineNr		ctermfg=241						cterm=none		guifg=#857b6f	guibg=#000000	gui=none
hi SpecialKey	ctermfg=241						cterm=none		guifg=#626262	guibg=#000000	gui=none
hi WarningMsg	ctermfg=203										guifg=#ff5f55
hi ErrorMsg		ctermfg=88		ctermbg=9		cterm=bold		guifg=#ff2026	guibg=#3a3a3a	gui=bold
hi MoreMsg		ctermfg=11						cterm=none

" Diff highlighting
hi DiffAdd						ctermbg=22										guibg=#008000
hi DiffDelete	ctermfg=15		ctermbg=88		cterm=none		guifg=#ffffff	guibg=#800000	gui=none
hi DiffChange					ctermbg=235										guibg=#808080

" Syntax Highlighting
hi Keyword		ctermfg=111		cterm=none		guifg=#88b8f6	gui=none
hi Statement	ctermfg=111		cterm=none		guifg=#88b8f6	gui=none
hi Constant		ctermfg=173		cterm=none		guifg=#e5786d	gui=none
hi Number		ctermfg=173		cterm=none		guifg=#e5786d	gui=none
hi PreProc		ctermfg=173		cterm=none		guifg=#e5786d	gui=none
hi Function		ctermfg=192		cterm=none		guifg=#cae982	gui=none
hi Identifier	ctermfg=166		cterm=none		guifg=#FF9900	gui=none
hi Type			ctermfg=186		cterm=none		guifg=#d4d987	gui=none
hi Special		ctermfg=229		cterm=none		guifg=#eadead	gui=none
hi String		ctermfg=113		cterm=none		guifg=#95e454	gui=italic
hi Comment		ctermfg=246		cterm=none		guifg=#9c998e	gui=italic
hi Todo			ctermfg=101		ctermbg=0		cterm=none		guifg=#857b6f	gui=italic
hi htmlBold		ctermfg=106						guifg=#90C107
hi htmlItalic	ctermfg=073						guifg=#72CCD8

" Linking
hi! link FoldColumn		Folded
hi! link CursorColumn	CursorLine
hi! link NonText		LineNr
hi! link DiffText		DiffChange
hi! link SpellBad		ErrorMsg
hi! link Error			ErrorMsg
hi! link Question		MoreMsg

" Unhighlighted:
" CursorIM
" CursorColumn
" DiffText
" Directory
" FoldColumn
" IncSearch
" Menu
" ModeMsg
" MoreMsg
" NonText
" PmenuSbar
" PmenuThumb
" Question
" Scrollbar
" SignColumn
" SpellBad
" SpellCap
" SpellLocal
" SpellRare
" TabLine
" TabLineFill
" TabLineSel
" Tooltip
" User1
" User9
" WildMenu
" Links
"
" vim: ts=4:sw=4:noet
