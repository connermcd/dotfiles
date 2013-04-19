" --- hipster ---
" Author: Conner McDaniel (connermcd.com)
if version > 580
   hi clear
   if exists("syntax_on")
      syntax reset
   endif
endif

let g:colors_name = "hipster"
set t_Co=256
set background=dark

" Vim >= 7.0 specific colors
if version >= 700
hi CursorLine     guifg=NONE      guibg=#32322f   guisp=NONE      gui=NONE   ctermfg=NONE   ctermbg=236    cterm=NONE
hi CursorLineNr   guifg=#8a8a8a   guibg=#32322f   guisp=NONE      gui=NONE   ctermfg=245    ctermbg=236    cterm=NONE
hi MatchParen     guifg=#eae788   guibg=#857b6f   guisp=NONE      gui=bold   ctermfg=228    ctermbg=101    cterm=bold
hi PMenu          guifg=#dddddd   guibg=#423d35   guisp=#423d35   gui=NONE   ctermfg=253    ctermbg=238    cterm=NONE
hi PMenuSbar      guifg=NONE      guibg=#848688   guisp=#848688   gui=NONE   ctermfg=NONE   ctermbg=102    cterm=NONE
hi PMenuSel       guifg=#ffd700   guibg=#706070   guisp=#706070   gui=bold   ctermfg=220    ctermbg=242    cterm=bold
hi PMenuThumb     guifg=NONE      guibg=#a4a5a8   guisp=#a4a5a8   gui=NONE   ctermfg=NONE   ctermbg=248    cterm=NONE
endif

" General colors
hi Normal         guifg=#f9f8ff   guibg=#000000   guisp=NONE      gui=NONE   ctermfg=15     ctermbg=NONE   cterm=NONE
hi Cursor         guifg=NONE      guibg=#cd6f5c   guisp=#cd6f5c   gui=NONE   ctermfg=NONE   ctermbg=173    cterm=NONE
hi Visual         guifg=#c3c6ca   guibg=#554d4b   guisp=NONE      gui=NONE   ctermfg=251    ctermbg=239    cterm=NONE
hi Visualnos      guifg=#c3c6ca   guibg=#303030   guisp=NONE      gui=NONE   ctermfg=251    ctermbg=236    cterm=NONE
hi Search         guifg=#000000   guibg=#8dabcd   guisp=#8dabcd   gui=NONE   ctermfg=NONE   ctermbg=110    cterm=NONE
hi Folded         guifg=#857b6f   guibg=#000000   guisp=NONE      gui=NONE   ctermfg=241    ctermbg=233    cterm=NONE
hi StatusLineNC   guifg=NONE      guibg=#262626   guisp=#262626   gui=NONE   ctermfg=NONE   ctermbg=235    cterm=NONE
hi VertSplit      guifg=#444444   guibg=#444444   guisp=NONE      gui=NONE   ctermfg=238    ctermbg=238    cterm=NONE
hi StatusLineNC   guifg=#857b6f   guibg=#444444   guisp=NONE      gui=NONE   ctermfg=241    ctermbg=238    cterm=NONE
hi LineNr         guifg=#595959   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=240    ctermbg=NONE   cterm=NONE
hi SpecialKey     guifg=#87beeb   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=117    ctermbg=NONE   cterm=NONE
hi WarningMsg     guifg=#bd4848   guibg=#f9f8ff   guisp=#f9f8ff   gui=bold   ctermfg=131    ctermbg=15     cterm=bold
hi ErrorMsg       guifg=#bd5353   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=131    ctermbg=NONE   cterm=NONE
hi MoreMsg        guifg=#ffff00   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=11     ctermbg=NONE   cterm=NONE

" Diff highlighting
hi DiffAdd        guifg=NONE      guibg=#301430   guisp=#3c664e   gui=NONE   ctermfg=NONE   ctermbg=236    cterm=NONE
hi DiffDelete     guifg=#ad3838   guibg=#301430   guisp=#301430   gui=NONE   ctermfg=131    ctermbg=236    cterm=NONE
hi DiffChange     guifg=NONE      guibg=#7e8c2d   guisp=#331833   gui=NONE   ctermfg=NONE   ctermbg=238    cterm=NONE

" Syntax highlighting
hi Keyword        guifg=#d6d69a   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=186    ctermbg=NONE   cterm=NONE
hi Function       guifg=#bf9b76   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=137    ctermbg=NONE   cterm=NONE
hi Constant       guifg=#44807d   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=66     ctermbg=NONE   cterm=NONE
hi Number         guifg=#386175   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=66     ctermbg=NONE   cterm=NONE
hi PreProc        guifg=#ad5234   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=131    ctermbg=NONE   cterm=NONE
hi Statement      guifg=#418db3   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=67     ctermbg=NONE   cterm=NONE
hi Identifier     guifg=#5f875f   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=65     ctermbg=NONE   cterm=NONE
hi Type           guifg=#babaa2   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=144    ctermbg=NONE   cterm=NONE
hi Special        guifg=#7a490d   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=3      ctermbg=NONE   cterm=NONE
hi String         guifg=#7e8c2d   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=100    ctermbg=NONE   cterm=NONE
hi Comment        guifg=#576157   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=241    ctermbg=NONE   cterm=NONE
hi Todo           guifg=#a1481e   guibg=NONE      guisp=NONE      gui=NONE   ctermfg=130    ctermbg=NONE   cterm=NONE

" Linking
hi! link          FoldColumn      Folded
hi! link          CursorColumn    CursorLine
hi! link          Search          CursorLine
hi! link          NonText         LineNr
hi! link          DiffText        DiffChange
hi! link          SpellBad        ErrorMsg
hi! link          SpellCap        ErrorMsg
hi! link          Error           ErrorMsg
hi! link          Question        MoreMsg
hi! link          htmlBold        Special
hi! link          htmlItalic      Number
hi! link          Title           Function

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

" vim: ts=3:sw=3:et
