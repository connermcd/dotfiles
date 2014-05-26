" Setup {{{1
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'JCLiang/vim-cscope-utils'
Bundle 'Valloric/YouCompleteMe'
Bundle 'benmills/vimux'
Bundle 'drmingdrmer/xptemplate'
Bundle 'godlygeek/tabular'
Bundle 'itchyny/calendar.vim'
Bundle 'jamessan/vim-gnupg'
Bundle 'scrooloose/syntastic'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-sleuth'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/FuzzyFinder'
Bundle 'vim-scripts/L9'
" Bundle 'kchmck/vim-coffee-script'
" Bundle 'tpope/vim-rails'
" Bundle 'vim-ruby/vim-ruby'
" Bundle 'elixir-lang/vim-elixir'

filetype plugin indent on
syntax enable

colorscheme hipster

" Options {{{1
set autoread
set backspace=2
set backup
set backupskip=/tmp/*,/private/tmp/*",*.gpg
set backupdir=~/.vim/tmp,/tmp
set browsedir=buffer
set directory=~/.vim/tmp,/tmp
set encoding=utf-8
set dictionary=~/.vim/spell/eng.utf-8.add
set expandtab
set fileencodings=utf-8
set fileformats=unix,dos,mac
set foldmethod=marker
" set formatprg=par
set hidden
set history=1000
set ignorecase
set incsearch
set list
set listchars=tab:▸\ ,eol:¬
set mouse=a
set noequalalways
set nohlsearch
set nojoinspaces
set number
set omnifunc=syntaxcomplete#Complete
set shiftround
set shiftwidth=4
set showcmd
set shortmess=filnxtToOI
set splitbelow
set splitright
set smartcase
set smartindent
set spelllang=eng
set tabstop=4
set tags=tags;$HOME
set timeoutlen=600
set ttyfast
set visualbell t_vb=".
set wildmode=list:longest,list:full
set wrapmargin=0
set wrap
" Necessary order
set linebreak
set textwidth=0
set display=lastline
" GUI settings {{{1
if has("gui_running")
   set guioptions-=T
   set guioptions-=r
   set guioptions-=R
   set guioptions-=m
   set guioptions-=l
   set guioptions-=L
   set guitablabel=%t
   " if Mac
   let s:uname = system("uname")
   if s:uname == "Darwin\n"
      set guifont=Monaco\ for\ Powerline
      set transparency=15
      let g:transparency = &transparency
      nnoremap gz :set fullscreen! columns=1000 transparency=0<cr>
   endif
else
   vnoremap <C-c> "+ygv"*y
   nnoremap <C-t> :tabnew<cr>
   nnoremap <RightMouse> "+]p
endif
" Leaders {{{1
nnoremap <leader>\ :w<cr>:mak<cr>
nnoremap <leader>. :cd %:h<cr>
nnoremap <leader>c :s/.*/\L&/<bar>:s/\<./\u&/g<cr>
nnoremap <leader>d "_d
nnoremap <leader>e :Errors<cr>
nnoremap <leader>q :q!<cr>
nnoremap <leader>s :set spell!<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>x :sign unplace *<cr>
nnoremap <leader>z :wq!<cr>
vnoremap <leader>d "_d
vnoremap <leader>q <esc>:q!<cr>
" Pandoc and Notes {{{1
command! -nargs=1 Ngrep vimgrep "<args>" $NOTES_DIR/**/*.txt
nnoremap <leader>[ :Ngrep 

command! -range=% Rst :'<,'>!pandoc -f markdown -t rst

nnoremap 'mh :w!<cr>:exe "!pandoc --latex-engine=lualatex -H ~/.cabal/fonts.tex -o " . fnameescape(expand('%:p:r')) . ".pdf " . fnameescape(expand('%:p'))<cr>
nnoremap 'md :w!<cr>:exe "!pandoc --latex-engine=lualatex -H ~/.cabal/fonts.tex -o $HOME/" . fnameescape(expand('%:t:r')) . ".pdf " . fnameescape(expand('%:p'))<cr>
nnoremap 'mp :w!<cr>:exe "!pandoc --latex-engine=lualatex -H ~/.cabal/fonts.tex -o /tmp/" . fnameescape(expand('%:t:r')) . ".pdf " . fnameescape(expand('%:p')) . " && xdg-open /tmp/" . fnameescape(expand('%:t:r')) . ".pdf"<cr>
" Misc {{{1
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
inoremap <bar>( <esc>d(s

" Type lang<C-Y> for shebang line
inoremap <C-y> <Esc>:sil exe ".!which <cWORD>" <bar> s/^/#!/ <bar> filetype detect<cr>YpDi
" Type 1. something<C-j> for 2.
inoremap <C-j> <esc>:exe "norm Ypf lDB\<C-a>"<cr>A

" Use :norm! so a count can be accepted
nnoremap <C-j> :norm! o<esc>k<cr>
nnoremap <C-k> :norm! O<esc>j<cr>

nnoremap <C-n> :cn<cr>z.
nnoremap <C-p> :cp<cr>z.
nnoremap Q :exe "try <bar> tabc! <bar> catch /E784/ <bar> qa! <bar> endtry"<cr>

nnoremap <leader>r :nnoremap <leader>r :!

" Steve Losh
noremap H ^
noremap L g_
noremap! <C-a> <Home>
noremap! <C-e> <End>

vnoremap K k
vnoremap & :s<cr>

command! W w !sudo tee % &>/dev/null
command! Mks let g:session = getcwd() <bar> call Mks(g:session)

" Pull last visually selected area onto command-line mode
cnoremap <C-R><C-V> <C-R>=fnameescape(getline("'<")[ getpos("'<")[2]-1 : getpos("'>")[2]-1 ])<CR>

" Drew Neil
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" Autocommands {{{1
augroup markdown " {{{2
   au!
   au BufEnter * let &complete=".,w,b,u,t,i"
   au BufNewFile,BufRead,BufWrite   *.txt,*.md,*.mkd,*.markdown,*.mdwn setl ft=markdown ts=3 sw=3
   au BufNewFile,BufRead,BufWrite   *.txt,*.md,*.mkd,*.markdown,*.mdwn let &complete="k".expand("%:p:h")."/*.md"
   au BufRead,BufWrite,InsertChange *.txt,*.md,*.mkd,*.markdown,*.mdwn syn match ErrorMsg '\%>77v.\+'
   au BufNewFile,BufRead */_posts/*.markdown setl completefunc=TagComplete | cd $BLOG/source
augroup end
augroup nonvim " {{{2
   au!
   au BufRead *.png,*.jpg,*.pdf,*.gif,*.xls*,*.scpt sil exe "!xdg-open " . shellescape(expand("%:p")) | bd | let &ft=&ft | redraw!
   " au BufRead *.ppt*,*.doc*,*.rtf sil exe "!xdg-open " . shellescape(expand("%:p")) | bd | let &ft=&ft | redraw!
   au BufRead *.ppt*,*.doc*,*.rtf let g:output_pdf = shellescape(expand("%:r") . ".pdf")
   au BufRead *.ppt*,*.doc*,*.rtf sil exe "!$HOME/.bin/pdf " . shellescape(expand("%:p"))
   au BufRead *.ppt*,*.doc*,*.rtf sil exe "!xdg-open " . g:output_pdf | bd | let &ft=&ft | redraw!
augroup end
augroup filetypes " {{{2
   au!
   au BufNewFile,BufRead,BufWrite todo.txt setl ft=todotxt
   au BufNewFile,BufRead,BufWrite *.csv setl ft=csv
   au BufNewFile,BufRead,BufWrite *.ejs setl ft=html
   au FileType calendar hi ExtraWhitespace NONE
   au FileType coffee  nnoremap <leader><leader> :CoffeeRun<cr>
   au FileType ruby    setl sw=2 makeprg=ruby\ % efm=
      \%+E%f:%l:\ parse\ error,
      \%W%f:%l:\ warning:\ %m,
      \%E%f:%l:in\ %*[^:]:\ %m,
      \%E%f:%l:\ %m,
      \%-C%\tfrom\ %f:%l:in\ %.%#,
      \%-Z%\tfrom\ %f:%l,
      \%-Z%p^,
      \%-G%.%#
   au FileType ruby    nnoremap <leader>p Yp^Cbinding.pry<Esc>
   au FileType python  setl sw=4 makeprg=python\ % efm=
      \%A\ \ File\ \"%f\"\\\,\ line\ %l\\\,%m,
      \%C\ \ \ \ %.%#,
      \%+Z%.%#Error\:\ %.%#,
      \%A\ \ File\ \"%f\"\\\,\ line\ %l,
      \%+C\ \ %.%#,
      \%-C%p^,
      \%Z%m,
      \%-G%.%#
   au FileType python  nnoremap <leader>p Yp^Cinteract()<Esc>
   au FileType xml     set equalprg=xmllint\ --format\ --recover\ -
   au FileType mail    let mapleader = "\\" | let maplocalleader = "," | setl spell fo=wantq1 smc=0
   au FileType cpp     set makeprg=g++\ \-lpcrecpp\ %\ &&\ ./a.out
   au FileType haskell set nocul cocu=in makeprg=ghc\ %
augroup end
augroup vimrc " {{{2
   au!
   au BufRead todo.txt setl ft=todotxt
   au BufWrite * sil !mkdir -p %:h
   au BufWritePost $MYVIMRC sil so $MYVIMRC
   au BufWritePost * sil FufRenewCache
   au BufRead *.session let g:session = getcwd() | so % | bd #
   au VimLeave * if exists("g:session") | call Mks(g:session) | endif
augroup end
" Highlight trailing whitespace " {{{2
highlight ExtraWhitespace guibg=#bd5353 ctermbg=131
augroup whitespace
  au!
  au ColorScheme * highlight ExtraWhitespace guibg=#bd5353 ctermbg=131
  au BufWinEnter * match ExtraWhitespace /\s\+$\| \+\ze\t/
  au BufWrite * match ExtraWhitespace /\s\+$\| \+\ze\t/
augroup end
" Plugins {{{1
" Fuzzy Finder {{{2
nnoremap '<Space> :FufBookmarkDir<cr>
nnoremap '.  :FufFileWithCurrentBufferDir<cr>
nnoremap ''  :b#<cr>
nnoremap '/  :FufFile /<cr>
nnoremap 'a  :FufFile app/<cr>
nnoremap 'd  :FufFile $HOME/Dropbox/<cr>
nnoremap 'f  :FufFile<cr>
nnoremap 'h  :FufFile $HOME/<cr>
nnoremap 'j  :FufFile $HOME/.vim/<cr>
nnoremap 'k  :FufBuffer<cr>
nnoremap 'l  :FufTag<cr>
nnoremap 'n  :FufFile $NOTES_DIR/<cr>
nnoremap 'p  :e! ${PASSWORD_FILE}.gpg<cr>
nnoremap 'r  :e! $HOME/.bashrc<cr><cr>
nnoremap 's  :FufFile spec/<cr>
nnoremap 't  :cd %:p:h<cr>:sh<cr>:cd -<cr>
nnoremap 'v  :e! $MYVIMRC<cr><cr>
nnoremap 'w  :FufFile $HOME/Dropbox/Tech/web/<cr>
nnoremap 'y  :FufFile $HOME/Dropbox/Archive/Bible/<cr>
let g:fuf_file_exclude = '\v\~$|\.(DS_Store|o|exe|dll|bak|orig|swp)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])'
" xptemplate {{{2
let g:xptemplate_debug_log = '~/.xpt.log'
" Surround.vim {{{2
let g:surround_42 = "**\r**"
nnoremap ** :exe "norm v$hS*"<cr>
nnoremap __ :exe "norm v$hS_"<cr>
vmap * S*
vmap _ S_
vmap <leader>l <Plug>VSurround]%a(<C-r><C-p>+)<Esc>
" tComment {{{2
let g:tcommentGuessFileType_markdown = 'html'
" Syntastic {{{2
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_enable_highlighting = 0
" YouCompleteMe {{{2
let g:ycm_global_ycm_extra_conf = '$HOME/.vim/bundle/YouCompleteMe/cpp/ycm/.ycm_extra_conf.py'
" UltiSnips {{{2
" function! g:UltiSnips_Complete()
" call UltiSnips_ExpandSnippet()
" if g:ulti_expand_res == 0
"     if pumvisible()
"         return "\<C-n>"
"     else
"         call UltiSnips_JumpForwards()
"         if g:ulti_jump_forwards_res == 0
"            return "\<TAB>"
"         endif
"     endif
" endif
" return ""
" endfunction
"
" au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
" let g:UltiSnipsJumpForwardTrigger="<tab>"
" let g:UltiSnipsListSnippets="<c-r><tab>"
" calendar.vim {{{2
let g:calendar_google_calendar = 1
let g:calendar_clock_12hour = 1
" Functions {{{1
" HTML Paste {{{2
command! -range=% HtmlPaste <line1>,<line2>call HtmlPaste()
noremap <silent> gH :HtmlPaste<cr>
fun! HtmlPaste() range
   exe a:firstline.",".a:lastline."TOhtml"
   let curTime = strftime("%H%M%S%m%d%y")
   exe "%s#</style>#</style>
       \<script src='//ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js'></script>
       \<script type='text/javascript'>
       \$(document).ready(function(){\r
       \   $('.LineNr').hide();\r
       \   $('.Folded').hide();\r
       \   if (window.location.href.match(/lines=1/))\r
       \      $('.LineNr').show();\r
       \   if (window.location.href.match(/folds=1/))\r
       \      $('.Folded').show();\r
       \});
       \</script>#"
   exe "sav! $BLOG/source/paste/" . curTime . ".html"
   wincmd c
   exe "silent !rsync -a --del $BLOG/source/paste connermcd@connermcd.com:/web"
   exe "silent !echo \"http://connermcd.com/paste/" . curTime . ".html\" | xclip"
   redraw!
endfun
" Tabularize {{{2
vnoremap <leader>t j:call <SID>table()<cr>
inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a
fun! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfun
fun! s:table() range
   exe "'<,'>Tab /|"
   let hsepline= substitute(getline("."),'[^|]','-','g')
   exe "norm! o" .  hsepline
   exe "'<,'>s/-|/ |/g"
   exe "'<,'>s/|-/| /g"
   exe "'<,'>s/^| \\|\\s*|$\\||//g"
endfun
" Yanklist {{{2
command! -nargs=* Y :call YankList(<f-args>)
fun! YankList(...)
   let yanklist = []
   for i in a:000
      if match(i, "-") != -1
         let split = split(i, "-")
         let yanklist = yanklist + range(split[0], split[1])
      else
         let yanklist = yanklist + i
      endif
   endfor
   call setreg('"', "")
   echo string(yanklist)
   for i in yanklist
      call setreg('"', getline(i), "al")
   endfor
endfun
" TagComplete {{{2
fun! TagComplete(findstart, base)
  if a:findstart
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ '\a'
      let start -= 1
    endwhile
    return start
  else
    let tags = split(system("ls $BLOG/public/blog/tags"), "\n")
    let cats = split(system("ls $BLOG/public/blog/categories"), "\n")
    let res = []
    for m in tags + cats
       if m =~ '^' . a:base
         call add(res, m)
       endif
    endfor
    return res
  endif
endfun
" NewPost {{{2
nnoremap 'bb :FufFile $BLOG/source/_posts/<cr>
nnoremap 'bn :NewPost 
nnoremap 'bp :!cd $BLOG && rake gen_deploy && cd -<cr>
command! -nargs=1 NewPost call NewPost("<args>")
fun! NewPost(args)
   let file = "$BLOG/source/_posts/" . strftime("%Y-%m-%d") . "-" . tolower(substitute(a:args, " ", "-", "g")) . ".markdown"
   exe "e!" . file
   let g:post_title = a:args
endfun
" Handle URIs {{{2
nnoremap go :call HandleURI()<cr>
fun! HandleURI()
  let l:uri = matchstr(getline("."), '[a-z]*:\/\/[^ >,;]*')
  if l:uri != ""
      if has("win32")
          exec "silent !start rundll32.exe url.dll,FileProtocolHandler " . l:uri
      else
          exec "silent !xdg-open \"" . fnameescape(l:uri) . "\""
      endif
      echo "Opened URI: " . l:uri
  else
      echo "No URI found in line."
  endif
  redraw!
endfun
" PrettyJSON {{{2
command! -range=% JSON <line1>,<line2>call PrettyJSON()
fun! PrettyJSON() range
   exe a:firstline . "," . a:lastline . "!python2 -mjson.tool"
endfun
" Mksession {{{2
fun! Mks(path)
   exe "mksession! ".a:path."/".fnamemodify(a:path, ':t').".session"
endfun
" Greek {{{1
map! <C-v>GA Γ
map! <C-v>DE Δ
map! <C-v>TH Θ
map! <C-v>LA Λ
map! <C-v>XI Ξ
map! <C-v>PI Π
map! <C-v>SI Σ
map! <C-v>PH Φ
map! <C-v>PS Ψ
map! <C-v>OM Ω
map! <C-v>al α
map! <C-v>be β
map! <C-v>ga γ
map! <C-v>de δ
map! <C-v>ep ε
map! <C-v>ze ζ
map! <C-v>et η
map! <C-v>th θ
map! <C-v>io ι
map! <C-v>ka κ
map! <C-v>la λ
map! <C-v>mu μ
map! <C-v>xi ξ
map! <C-v>pi π
map! <C-v>rh ρ
map! <C-v>si σ
map! <C-v>ta τ
map! <C-v>ps ψ
map! <C-v>om ω
map! <C-v>ph ϕ
" Math {{{1
map! <C-v>ll →
map! <C-v>hh ⇌
map! <C-v>kk ↑
map! <C-v>jj ↓
map! <C-v>= ∝
map! <C-v>~ ≈
map! <C-v>!= ≠
map! <C-v>!> ⇸
map! <C-v>~> ↝
map! <C-v>>= ≥
map! <C-v><= ≤
map! <C-v>0  °
map! <C-v>ce ¢
map! <C-v>*  •
map! <C-v>co ⌘
" Subscript and Superscript {{{1
inoremap <leader>1 ~1~
inoremap <leader>2 ~2~
inoremap <leader>3 ~3~
inoremap <leader>4 ~4~
inoremap <leader>5 ~5~
inoremap <leader>6 ~6~
inoremap <leader>7 ~7~
inoremap <leader>8 ~8~
inoremap <leader>9 ~9~
inoremap <leader>== ^+^
inoremap <leader>=2 ^2+^
inoremap <leader>=3 ^3+^
inoremap <leader>-- ^-^
inoremap <leader>-2 ^2-^
inoremap <leader>-3 ^3-^
" Extended Text Objects {{{1
let items = [ "<bar>", "\\", "/", ":", ".", "*", "_" ]
for item in items
  exe "nnoremap yi".item." T".item."yt".item
  exe "nnoremap ya".item." F".item."yf".item
  exe "nnoremap ci".item." T".item."ct".item
  exe "nnoremap ca".item." F".item."cf".item
  exe "nnoremap di".item." T".item."dt".item
  exe "nnoremap da".item." F".item."df".item
  exe "nnoremap vi".item." T".item."vt".item
  exe "nnoremap va".item." F".item."vf".item
endfor
nnoremap viz v[zo]z$

" }}} vim: fdm=marker
