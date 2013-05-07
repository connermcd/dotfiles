# Setup {{{1
stty -ixon -ixoff # turns off CTRL-S
[[ $- != *i* ]] && return

if [ `hostname` == "connermcd-laptop" ]; then
   PS1="\[\e[1;36m\]\W\[\e[1;31m\]:\[\e[0m\] "
else
   PS1="\[\e[0;37m\]@\[\e[1;30m\]$(hostname) \[\e[1;36m\]\W\[\e[1;31m\]:\[\e[0m\] "
fi

bind TAB:menu-complete
# Env {{{1
export BROWSER=/usr/bin/chromium
export EDITOR=vim
export HISTSIZE=1000
export BLOG=$HOME/Dropbox/Tech/projects/ruby/octopress
# Notes {{{2
export YEAR=2
export MODULE=9
export NOTES_DIR=$HOME/Dropbox/Courses/Notes
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
alias py="python"
alias py2="python2"
alias r="rails"
alias tm="tmux"
alias tma="tmux attach"
alias trans="transmission-cli"
alias unneeded="sudo pacman -Rsn \$(pacman -Qqdt)"
alias wifi="sudo wifi-menu"
# builtin {{{2
alias duh="du -h -d 0 [^.]*"
alias l="ls -al"
alias ls='ls --color=auto'
alias myip="curl http://myip.dnsomatic.com && echo ''"
alias syms="find . -maxdepth 1 -type l -print | while read line; do ls -alc "\$line"; done"
# flags {{{2
alias bd="cd $BLOG && bundle exec rake gen_deploy && cd -"
alias pandoc="pandoc --latex-engine=lualatex"
alias pretty-json="python2 -mjson.tool"
alias screencast-external="ffmpeg -f alsa -ac 2 -i hw:1,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias screencast-internal="ffmpeg -f alsa -ac 2 -i hw:0,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias screencast-sys-out="ffmpeg -f alsa -ac 2 -i hw:0,1 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias webcast-external="ffmpeg -f alsa -ac 2 -i hw:1,0 -f v4l2 -itsoffset 1 -s 640x480 -i /dev/video0 -acodec pcm_s16le -vcodec libx264 -y output.mkv"
alias wifi="wicd-cli -y"
# alias sudo="sudo -E"
# gcalcli {{{2
alias gc="gcalcli --cal \"Conner McDaniel\" --cal \"CMDA SATX\" --cal \"UTHSCSA\""
alias gcb="gcalcli --cal \"Conner McDaniel\" --cal \"UTHSCSA\""
alias gcm="gcalcli --cal \"Conner McDaniel\""
alias gce="gcalcli --cal \"Campus Events\""
alias gcu="gcalcli --cal \"UTHSCSA\""
alias today="gcb --detail-descr agenda 12am 11:59pm"
# libreoffice {{{2
alias writer="libreoffice --writer"
alias calc="libreoffice --calc"
alias impress="libreoffice --impress"
alias draw="libreoffice --draw"
# Functions {{{1
function speed {
   </dev/null ffmpeg -i "$*" -filter atempo=1.5 "${*%%.mp3}-150.mp3"
}
function nack {
   vim -c "Nack $*"
}
function t {
   todo=/home/connermcd/Dropbox/Tech/todo/todo.sh
   if [ "$1" == "edit" ]; then
      $todo $1
   else
      $todo $* | less -FXRS
   fi
}
function webrick {
   port="${1:-3000}"
   ruby -r webrick -e "s = WEBrick::HTTPServer.new(:Port => $port, :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}
function pacsize {
   sudo pacman -Qi | awk 'BEGIN{sort="sort -k2 -n"} /Name/ {name=$3} /Size/ {size=$4/1024;print name":",size,"Mb"|sort}' | less
}
function ptime {
   find -type f -name \"*\" -print0 | xargs -0  mplayer -vo dummy -ao dummy -identify 2>/dev/null | perl -nle '/ID_LENGTH=([0-9\.]+)/ && (\$t +=\$1) && printf \"%02d:%02d:%02d\n\",\$t/3600,\$t/60%60,\$t%60' | tail -n 1
}
# Path {{{1
pathadd() {
if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    PATH="${PATH:+"$PATH:"}$1"
fi
}
pathadd $HOME/.bin
pathadd $HOME/.cabal/bin
pathadd $HOME/.rbenv/bin
pathadd $HOME/.gem/ruby/2.0.0/bin
eval "$(rbenv init -)"
# }}} vim: fdm=marker
