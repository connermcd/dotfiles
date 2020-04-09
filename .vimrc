" Setup {{{1
set nocompatible

colorscheme apprentice

filetype plugin indent on
syntax enable

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
set formatprg=par
set hidden
set history=1000
set ignorecase
set incsearch
set laststatus=1
set list
set listchars=tab:▸\ ,eol:¬
set mouse=a
set noequalalways
set nohlsearch
set nojoinspaces
set number
set omnifunc=syntaxcomplete#Complete
set path+=**
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
set timeoutlen=600
set ttyfast
set visualbell t_vb=".
set wildcharm=<C-z>
set wildignorecase
set wildmenu
set wildmode=longest,list
set wrapmargin=0
set wrap
" Necessary order
set linebreak
set textwidth=0
set display=lastline
" GUI options {{{2
if has("gui_running")
    set guioptions-=T
    set guioptions-=r
    set guioptions-=R
    set guioptions-=m
    set guioptions-=l
    set guioptions-=L
    set guitablabel=%t
endif
" Variables {{{2
let g:python_host_prog  = '/usr/bin/python2'
let g:python3_host_prog = '/usr/bin/python3'
" Plugin Options {{{1
" vim-surround {{{2
let g:surround_42 = "**\r**"
nnoremap ** :exe "norm v$hS*"<cr>
nnoremap __ :exe "norm v$hS_"<cr>
vmap * S*
vmap _ S_
vmap <leader>l <Plug>VSurround]%a(<C-r><C-p>+)<Esc>
" syntastic {{{2
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_enable_highlighting = 0
let g:syntastic_java_javac_config_file_enabled = 1
" YouCompleteMe {{{2
let g:ycm_global_ycm_extra_conf = '$HOME/.vim/bundle/YouCompleteMe/cpp/ycm/.ycm_extra_conf.py'
let g:ycm_filetype_blacklist = {
\ 'pdf' : 1,
\}
" Tagbar {{{2
let g:tagbar_width = 80
let g:tagbar_sort = 0
" vim-pandoc {{{2
let g:pandoc#filetypes#handled = ["pandoc", "markdown", "textile"]
let g:pandoc#biblio#use_bibtool = 1
let g:pandoc#completion#bib#mode = 'citeproc'
let g:pandoc#biblio#bibs = ["articles/bib.bib"]
let g:pandoc#syntax#conceal#use = 0
let g:pandoc#folding#fdc = 0
let g:pandoc#folding#level = 999
" netrw {{{2
let g:netrw_http_cmd = "qutebrowser"
let g:netrw_browsex_viewer = "xdg-open"
" Pandoc and Notes {{{2
command! -nargs=1 Ngrep lvimgrep "<args>" $NOTES_DIR/**/*.txt
nnoremap <leader>[ :Ngrep 

command! -range=% Rst :'<,'>!pandoc -f markdown -t rst

nnoremap 'ms :w!<cr>:exe "!pandoc -t beamer -o " . fnameescape(expand('%:p:r')) . ".pdf " . fnameescape(expand('%:p'))<cr>
nnoremap 'mh :w!<cr>:exe "!pandoc --pdf-engine=lualatex -H ~/.config/pandoc/fonts.tex -o " . fnameescape(expand('%:p:r')) . ".pdf " . fnameescape(expand('%:p'))<cr>
nnoremap 'md :w!<cr>:exe "!pandoc --pdf-engine=lualatex -H ~/.config/pandoc/fonts.tex -o $HOME/" . fnameescape(expand('%:t:r')) . ".pdf " . fnameescape(expand('%:p'))<cr>
nnoremap 'mp :w!<cr>:exe "!pandoc --pdf-engine=lualatex -H ~/.config/pandoc/fonts.tex -o /tmp/" . fnameescape(expand('%:t:r')) . ".pdf " . fnameescape(expand('%:p')) . " && xdg-open /tmp/" . fnameescape(expand('%:t:r')) . ".pdf"<cr>
" Extended Text Objects {{{2
let s:items = [ "<bar>", "\\", "/", ":", ".", "*", "_" ]
for item in s:items
    exe "nnoremap yi".item." T".item."yt".item
    exe "nnoremap ya".item." F".item."yf".item
    exe "nnoremap ci".item." T".item."ct".item
    exe "nnoremap ca".item." F".item."cf".item
    exe "nnoremap di".item." T".item."dt".item
    exe "nnoremap da".item." F".item."df".item
    exe "nnoremap vi".item." T".item."vt".item
    exe "nnoremap va".item." F".item."vf".item
endfor
" Select within fold
nnoremap viz v[zo]z$
" Mappings {{{1
" File navigation {{{2
" . = location of current file
nnoremap '.  :exe ":FZF " . expand("%:h")<CR>
" / = /
nnoremap '/  :e /<C-d>
" b = buffers
nnoremap 'b  :Buffers<cr>
" c = config
nnoremap 'c  :FZF ~/.config/<cr>
" d = documents
nnoremap 'd  :FZF ~/Documents/<cr>
" f = fzf
nnoremap 'f  :FZF<cr>
" g = grep (ripgrep)
nnoremap 'g  :Rg 
" h = home
nnoremap 'h  :FZF ~/<cr>
" n = notes
nnoremap 'n  :FZF ~/Documents/Notes/<cr>
" t = tags
nnoremap 't  :Tags<cr>
" r = bashrc
nnoremap 'r  :e ~/.bashrc<cr>
" v = vimrc
nnoremap 'v  :e $MYVIMRC<cr>
" toggle last buffer
nnoremap ''  :b#<cr>
" Leaders {{{2
nnoremap <leader>\ :!chmod +x %<cr>:!%:p<cr>
nnoremap <leader>. :cd %:h<cr>
inoremap <leader>d <C-r>=strftime('%D %l:%M%P')<cr>
inoremap <leader>D <C-r>=strftime('%D')<cr>
nnoremap <leader>d "_d
inoremap <leader>i import code; code.interact(local=dict(globals(), **locals()))<esc>
nnoremap <leader>n :exe "e ~/Documents/Notes/Scratch/stash/".strftime("%F-%H%M%S").".md"<cr>
nnoremap <leader>q :q!<cr>
nnoremap <leader>s :set spell!<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>x :sign unplace *<cr>
nnoremap <leader>z :wq!<cr>
vnoremap <leader>d "_d
vnoremap <leader>q <esc>:q!<cr>
" Misc {{{2

" Type lang<C-Y> for shebang line
inoremap <C-y> <Esc>:sil exe ".!which <cWORD>" <bar> s/^/#!/ <bar> filetype detect<cr>YpDi

" Type 1. something<C-j> for 2.
inoremap <C-j> <esc>:exe "norm Ypf lDB\<C-a>"<cr>A

" Use :norm! so a count can be accepted
nnoremap <C-j> :norm! o<esc>k<cr>
nnoremap <C-k> :norm! O<esc>

" Various
vnoremap <C-c> "+ygv"*y
nnoremap <C-t> :tabnew<cr>
nnoremap <RightMouse> "+]p
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
inoremap <bar>( <esc>d(s
nnoremap <C-n> :cnext<cr>z.
nnoremap <C-p> :cprev<cr>z.
" nnoremap <C-S-n> :lnext<cr>z.
" nnoremap <C-S-p> :lprev<cr>z.
nnoremap Q :qa!<cr>

" Steve Losh
noremap H ^
noremap L g_

vnoremap K k
vnoremap & :s<cr>

command! W w !sudo tee % &>/dev/null
command! Mks let g:session = getcwd() <bar> call Mks(g:session)

" Pull last visually selected area onto command-line mode
cnoremap <C-r><C-v> <C-R>=fnameescape(getline("'<")[ getpos("'<")[2]-1 : getpos("'>")[2]-1 ])<CR>

" Drew Neil
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Autocommands {{{1
highlight ExtraWhitespace guibg=#bd5353 ctermbg=131
augroup vimrc
    au!
    au BufWrite * sil !mkdir -p %:h
    au BufWritePost $MYVIMRC sil so $MYVIMRC
    au BufWritePost .Xresources sil call system('xrdb ~/.Xresources')
    au ColorScheme * highlight ExtraWhitespace guibg=#bd5353 ctermbg=131
    au BufWinEnter * match ExtraWhitespace /\s\+$\| \+\ze\t/
    au BufWrite * match ExtraWhitespace /\s\+$\| \+\ze\t/
    au BufNewFile,BufRead */_posts/*.markdown setl completefunc=TagComplete | cd $BLOG
augroup end
" Functions {{{1
" Pack {{{2
command! PackUpdate echo system('find ~/.vim/pack/bundle/start/*/. -maxdepth 0 -execdir pwd \; -execdir git pull \;')
command! PackList echo system('ls ~/.vim/pack/bundle/start')
command! -nargs=1 PackInstall echo system('cd ~/.vim/pack/bundle/start && git clone git@github.com:<args>.git')
command! -nargs=1 PackUninstall echo system('rm -rf ~/.vim/pack/bundle/start/<args>')
" Ix {{{2
command! -range=% Ix :<line1>,<line2>call Ix()
fun! Ix() range
    call setreg("+", system('ix', join(getline(a:firstline, a:lastline), "\n")))
endfun
" NewPost {{{2
command! -nargs=1 NewPost call NewPost("<args>")
fun! NewPost(args)
    let l:file = "$BLOG/_posts/" . strftime("%Y-%m-%d") . "-" . tolower(substitute(a:args, " ", "-", "g")) . ".markdown"
    exe "e!" . l:file
    put ='---'
    put ='date: '''.strftime("%Y-%m-%d H:M:S").''''
    put ='layout: post'
    put ='slug: '.l:file
    put ='title: '.a:args
    put ='categories:'
    put ='- blog'
    put ='---'
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
" Symbol Shortcuts {{{1
" Greek {{{2
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
" Math {{{2
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
" Subscript and Superscript {{{2
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

" }}} vim: fdm=marker
