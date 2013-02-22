" Setup {{{1
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'vim-scripts/FuzzyFinder'
Bundle 'vim-scripts/L9'
Bundle 'mileszs/ack.vim'
Bundle 'scrooloose/syntastic'
Bundle 'vim-scripts/tComment'
Bundle 'godlygeek/tabular'
Bundle 'tpope/vim-fugitive'
Bundle 'jamessan/vim-gnupg'
Bundle 'tpope/vim-sleuth'
Bundle 'tpope/vim-surround'
Bundle 'benmills/vimux'
Bundle 'int3/vim-taglist-plus'
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-rails'

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
set cursorline
set directory=~/.vim/tmp,/tmp
set encoding=utf-8
set dictionary=~/.vim/spell/eng.utf-8.add
set expandtab
set fileencodings=utf-8
set fileformats=unix,dos,mac
set foldmethod=marker
set formatprg=par
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
set showcmd
set shortmess=filnxtToOI
set smartcase
set smartindent
set spelllang=eng
set timeoutlen=600
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
inoremap <leader>\ \<esc>a
inoremap <leader>l <esc>:TlistToggle<cr>
inoremap <leader>q <esc>:q!<cr>
inoremap <leader>s <esc>:set spell!<cr>a
inoremap <leader>w <esc>:w<cr>a
inoremap <leader>x <esc>:sign unplace *<cr>a
inoremap <leader>z <esc>:wq!<cr>
nnoremap <leader>\ :w<bar>mak<cr>
nnoremap <leader>. :cd %:h<cr>
nnoremap <leader>c :s/.*/\L&/<bar>:s/\<./\u&/g<cr>
nnoremap <leader>d "_d
nnoremap <leader>l :TlistToggle<cr>
nnoremap <leader>q :q!<cr>
nnoremap <leader>s :set spell!<cr>
nnoremap <leader>u :w!<cr>:exe "!unotes"<cr>:redraw!<cr>:echo "Notes updated."<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>x :sign unplace *<cr>
nnoremap <leader>z :wq!<cr>
vnoremap <leader>d "_d
vnoremap <leader>q <esc>:q!<cr>
" Pandoc and Notes {{{1
let g:module = system('echo -n "$MODULE"')
command! -nargs=1 Nls Ack -i --text --nohtml "<args>" $HOME/Dropbox/Notes/**/*.txt
command! -nargs=1 Note exe "e! " . fnameescape($HOME."/Dropbox/Notes/mod" . g:module . "/<args>.txt")
command! -range=% Rst :'<,'>!pandoc -f markdown -t rst

nnoremap <leader>[ :Nls 
nnoremap <leader>] :Note 

nnoremap 'mh :w!<cr>:exe "!pandoc --latex-engine=lualatex -H $HOME/Dropbox/Notes/fonts.tex -o " . fnameescape(expand('%:p:r')) . ".pdf " . fnameescape(expand('%:p'))<cr>
nnoremap 'md :w!<cr>:exe "!pandoc --latex-engine=lualatex -H $HOME/Dropbox/Notes/fonts.tex -o $HOME/" . fnameescape(expand('%:t:r')) . ".pdf " . fnameescape(expand('%:p'))<cr>
" Misc {{{1
inoremap <C-j> <esc>:exe "norm Ypf lDB\<C-a>"<cr>A
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
inoremap <bar>( <esc>d(s

" Type lang<C-Y> for shebang line
inoremap <C-Y> <Esc>:sil exe ".!which <cWORD>" <bar> s/^/#!/ <bar> filetype detect<cr>YpDi

nnoremap <C-j> o<esc>
nnoremap <C-k> O<esc>

nnoremap dg :diffget 

nnoremap <C-n> :cn<cr>z.
nnoremap <C-p> :cp<cr>z.
nnoremap Q :exe "try <bar> tabc! <bar> catch /E784/ <bar> qa! <bar> endtry"<cr>
vnoremap K k
xnoremap p p:let @" = @0<cr>:<bs>
vnoremap & :s<cr>

" Remove duplicate lines (and empty lines unfortunately)
com! RemoveDuplicateLines %!awk '\!x[$0]++'

nnoremap g<C-n> gt
nnoremap g<C-p> gT
" nnoremap zp :norm! G{jzt13<C-y>G$<cr>
nnoremap zp Gzt13<C-y>G$
command! W w !sudo tee % &>/dev/null

" Pull last visually selected area onto command-line mode
cnoremap <C-R><C-V> <C-R>=fnameescape(getline("'<")[ getpos("'<")[2]-1 : getpos("'>")[2]-1 ])<CR>

" Steve Losh
noremap H ^
noremap L g_
noremap! <C-a> <Home>
noremap! <C-e> <End>

" Drew Neil
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" Autocommands {{{1
augroup markdown " {{{2
   au!
   " au BufEnter * if &filetype == "" && expand('%:t') != "__XtermColorTable__" | setl ft=markdown | endif
   au BufNewFile,BufRead,BufWrite *.txt,*.md,*.mkd,*.markdown,*.mdwn setl ft=markdown
   au BufNewFile,BufRead */_posts/*.markdown setl completefunc=TagComplete | cd $HOME/Dropbox/Tech/web/octopress/source
   " au BufRead,BufNewFile,BufEnter */mod*/*.txt let &complete="k$HOME/Dropbox/Notes/mod".g:module."/*.txt"
   au BufEnter * let &complete=".,w,b,u,t,i"
   au BufRead,BufNewFile,BufEnter   */mod*/*.txt let &complete="k$HOME/Dropbox/Notes/**/*.txt"
   au BufRead,BufNewFile,BufEnter   */mod*/*.txt lcd %:h
   au BufRead,BufWrite,InsertChange */mod*/*.txt syn match ErrorMsg '\%>77v.\+'
augroup end
augroup nonvim " {{{2
   au!
   au BufRead *.png,*.jpg,*.pdf,*.gif,*.xls*,*.scpt sil exe "!xdg-open " . shellescape(expand("%:p")) | bd | let &ft=&ft | redraw!
   au BufRead *.ppt*,*.doc*,*.rtf sil exe "!xdg-open " . shellescape(expand("%:p")) | bd | let &ft=&ft | redraw!
   " au BufRead *.ppt*,*.doc*,*.rtf let g:output_pdf = shellescape(expand("%:r") . ".pdf")
   " au BufRead *.ppt*,*.doc*,*.rtf sil exe "!/usr/local/bin/any2pdf " . shellescape(expand("%:p"))
   " au BufRead *.ppt*,*.doc*,*.rtf sil exe "!xdg-open " . g:output_pdf | bd | let &ft=&ft | redraw!
augroup end
augroup filetypes " {{{2
   au!
   au BufNewFile,BufRead,BufWrite *.csv setl ft=csv
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
   au FileType cpp     set makeprg=g++\ %\ &&\ ./a.out
   au FileType haskell set nocul cocu=in
   " au FileType help    set iskeyword=!-~,^*,^|,^"
augroup end
augroup vimrc " {{{2
   au!
   au BufWritePost $MYVIMRC sil so $MYVIMRC
   au BufWritePost * sil FufRenewCache
augroup end
" Plugins {{{1
" Fuzzy Finder {{{2
nnoremap '.  :FufFileWithCurrentBufferDir<cr>
nnoremap ''  :b#<cr>
nnoremap '/  :FufFile /<cr>
nnoremap 'a  :FufFile app/<cr>
nnoremap 'c  :FufFile $HOME/Dropbox/Courses/Module <c-r>=g:module<cr>/<cr>
nnoremap 'd  :FufFile $HOME/Dropbox/<cr>
nnoremap 'f  :FufFile<cr>
nnoremap 'h  :FufFile $HOME/<cr>
nnoremap 'j  :FufFile $HOME/.vim/<cr>
nnoremap 'k  :FufBuffer<cr>
nnoremap 'l  :FufTag<cr>
nnoremap 'n  :FufFile $HOME/Dropbox/Notes/mod<c-r>=g:module<cr>/<cr>
nnoremap 'p  :e! $HOME/Dropbox/Archive/Important/passwords.gpg<cr>
nnoremap 'r  :e! $HOME/.bashrc<cr><cr>
nnoremap 's  :FufFile $HOME/.bin/<cr>
nnoremap 't  :cd %:p:h<cr>:sh<cr>:cd -<cr>
nnoremap 'v  :e! $MYVIMRC<cr><cr>
nnoremap 'w  :FufDir $HOME/Dropbox/Tech/web/<cr>
nnoremap 'y  :FufFile $HOME/Dropbox/Archive/Bible/<cr>
let g:fuf_file_exclude = '\v\~$|\.(DS_Store|o|exe|dll|bak|orig|swp)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])'
" Surround.vim {{{2
let g:surround_42 = "**\r**"
nnoremap ** :exe "norm v$hS*"<cr>
nnoremap __ :exe "norm v$hS_"<cr>
vmap * S*
vmap _ S_
" taglist {{{2
let Tlist_Ctags_Cmd = "/usr/bin/ctags"
let Tlist_WinWidth = 50
let Tlist_Use_Right_Window = 1
let Tlist_File_Fold_Auto_Close = 1
let Tlist_Auto_Update = 1
let Tlist_Auto_Highlight_Tag = 1
" tComment {{{2
let g:tcommentGuessFileType_markdown = 'html'
" vimux {{{2
let g:VimuxResetSequence = "q C-u C-d"
" Run the current file with rspec
nnoremap <leader>rs :w<cr>:call VimuxRunCommand("clear; rspec " . bufname("%"))<CR>
" Run the current file with ruby
nnoremap <leader>rr :w<cr>:call VimuxRunCommand("clear; ruby " . bufname("%"))<CR>
" Run the current file with python3
nnoremap <leader>3 :w<cr>:call VimuxRunCommand("clear; python " . bufname("%"))<CR>
" Run the current file with python2
nnoremap <leader>2 :w<cr>:call VimuxRunCommand("clear; python2 " . bufname("%"))<CR>
" Run the current file with bash
nnoremap <leader>1 :w<cr>:call VimuxRunCommand("clear; bash " . bufname("%"))<CR>
" Prompt for a command to run
nnoremap <leader>rp :VimuxPromptCommand<CR>
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
   exe "sav! $HOME/Dropbox/Tech/web/octopress/source/paste/" . curTime . ".html"
   wincmd c
   exe "silent !rsync -a --del $HOME/Dropbox/Tech/web/octopress/source/paste connermcd@connermcd.com:/web"
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
    let tags = split(system("ls $HOME/Dropbox/Tech/web/octopress/public/blog/tags"), "\n")
    let cats = split(system("ls $HOME/Dropbox/Tech/web/octopress/public/blog/categories"), "\n")
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
nnoremap 'bb :FufFile $HOME/Dropbox/Tech/web/octopress/source/_posts/<cr>
nnoremap 'bn :NewPost 
nnoremap 'bp :!cd $HOME/Dropbox/Tech/web/octopress && rake gen_deploy && cd -<cr>
command! -nargs=1 NewPost call NewPost("<args>")
fun! NewPost(args)
   let file = "$HOME/Dropbox/Tech/web/octopress/source/_posts/" . strftime("%Y-%m-%d") . "-" . tolower(substitute(a:args, " ", "-", "g")) . ".markdown"
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
          exec "silent !open \"" . l:uri . "\"" 
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
map! <C-v>> →
map! <C-v>< ⇌
map! <C-v>n ↑
map! <C-v>v ↓
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
