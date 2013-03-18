# Setup {{{1
stty -ixon -ixoff # turns off CTRL-S
[[ $- != *i* ]] && return

if [ `hostname` == "connermcd-laptop" ]; then
   PS1='\[\e[1;36m\]\W\[\e[1;31m\]:\[\e[0m\] '
else
   PS1="\[\e[0;37m\]@\[\e[1;30m\]$(hostname) \[\e[1;36m\]\W\[\e[1;31m\]:\[\e[0m\] "
fi

bind TAB:menu-complete
# Env {{{1
export BROWSER=/usr/bin/chromium
export EDITOR=vim
export MODULE=8
export HISTSIZE=1000
# less colors {{{2
export LESS_TERMCAP_mb=$'\e[01;31m' # begin blinking
export LESS_TERMCAP_md=$'\e[01;34m' # begin bold
export LESS_TERMCAP_me=$'\e[0m'     # end mode
export LESS_TERMCAP_se=$'\e[0m'     # end standout-mode
export LESS_TERMCAP_so=$'\e[01;32m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\e[0m'     # end underline
export LESS_TERMCAP_us=$'\e[01;36m' # begin underline
# Aliases {{{1
# abbrevs {{{2
alias cleanvim="vim -N -u NONE"
alias em="mutt"
alias htop="sudo htop"
alias i="irssi"
alias linode="ssh connermcd@connermcd.com"
alias m="vimpc"
alias nstech="ssh connermcd@nstech"
alias open="xdg-open"
alias ptime="find -type f -name \"*\" -print0 | xargs -0  mplayer -vo dummy -ao dummy -identify 2>/dev/null | perl -nle '/ID_LENGTH=([0-9\.]+)/ && (\$t +=\$1) && printf \"%02d:%02d:%02d\n\",\$t/3600,\$t/60%60,\$t%60' | tail -n 1"
alias py="python"
alias py2="python2"
alias t="/home/connermcd/Dropbox/Tech/todo/todo.sh"
alias tm="tmux"
alias tma="tmux attach"
alias trans="transmission-cli"
alias unneeded="sudo pacman -Rsn \$(pacman -Qqdt)"
alias web="cd ~/Dropbox/Tech/web"
alias wifi="sudo wifi-menu"
# builtin {{{2
alias duh="du -h -d 0 [^.]*"
alias l="ls -al"
alias ls='ls --color=auto'
alias myip="curl http://myip.dnsomatic.com && echo ''"
alias syms="find . -maxdepth 1 -type l -print | while read line; do ls -alc "\$line"; done"
# flags {{{2
alias bd="cd $HOME/Dropbox/Tech/web/octopress && rake gen_deploy && cd -"
alias nack="ack --text --nohtml"
alias pandoc="pandoc --latex-engine=lualatex"
alias pretty-json="python2 -mjson.tool"
alias screencast="ffmpeg -f alsa -ac 2 -i hw:1,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias screencast-interal="ffmpeg -f alsa -ac 2 -i hw:0,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias wifi="wicd-cli -y"
# alias sudo="sudo -E"
# gcalcli {{{2
alias gc="gcalcli --cal \"Conner McDaniel\" --cal \"CMDA SATX\" --cal \"UTHSCSA\""
alias gcb="gcalcli --cal \"Conner McDaniel\" --cal \"UTHSCSA\""
alias gcm="gcalcli --cal \"Conner McDaniel\""
alias gce="gcalcli --cal \"Campus Events\""
alias gcu="gcalcli --cal \"UTHSCSA\""
alias today="gcu --detail-descr agenda 12am 11:59pm"
# libreoffice {{{2
alias writer="libreoffice --writer"
alias calc="libreoffice --calc"
alias impress="libreoffice --impress"
alias draw="libreoffice --draw"
# Functions {{{1
function speed {
   </dev/null ffmpeg -i "$*" -filter atempo=1.5 "${*%%.mp3}-150.mp3"
}
function nls {
   vim -c "Nls $*"
}
# Path {{{1
pathadd() {
if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    PATH="${PATH:+"$PATH:"}$1"
fi
}
pathadd $HOME/.bin
pathadd $HOME/.cabal/bin
pathadd $(ruby -rubygems -e "puts Gem.user_dir")/bin
# }}} vim: fdm=marker
