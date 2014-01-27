# Setup {{{1
stty -ixon -ixoff # turns off CTRL-S
[[ $- != *i* ]] && return

export PROMPT_COMMAND=__prompt_command

__prompt_command() {
   code=$?
   [[ $code != 0 ]] && echo -e "$RED✗ ${code}${RESET_COLOR}"
   PS1="$(ps1_hostname)\[\e[1;36m\]\W\[\e[1;31m\]:\[\e[0m\] "
}

ps1_hostname() {
   host=$(hostname)
   if [[ "$host" != "connermcd-laptop" ]]; then
      echo "\[\e[0;37m\]@\[\e[1;30m\]$host "
   fi
}

bind TAB:menu-complete
# Env {{{1
export BLOG="$HOME/Dropbox/Tech/src/rb/blog"
export BROWSER=/usr/bin/chromium
export EDITOR=vim
export HISTSIZE=1000
export PASSWORD_FILE="$HOME/Dropbox/Archive/Important/passwords"

export RED='\033[0;31m'
export GREEN='\033[0;32m'
export RESET_COLOR='\033[0m'
# Notes {{{2
export YEAR=2
export MODULE=9
export NOTES_DIR=$HOME/Dropbox/Archive/Med\ School/Notes
# less colors {{{2
export LESS_TERMCAP_mb=$'\e[01;31m' # begin blinking
export LESS_TERMCAP_md=$'\e[01;34m' # begin bold
export LESS_TERMCAP_me=$'\e[0m'     # end mode
export LESS_TERMCAP_se=$'\e[0m'     # end standout-mode
export LESS_TERMCAP_so=$'\e[01;32m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\e[0m'     # end underline
export LESS_TERMCAP_us=$'\e[01;36m' # begin underline
# Aliases {{{1
# abbrevs
alias v="vim"
alias i="irssi"
alias m="vimpc"
alias em="mutt"
alias r="ranger"
alias htop="sudo htop"
alias linode="ssh connermcd@linode"
alias monitor="sudo monitor"
alias nstech="ssh connermcd@nstech"
alias open="xdg-open"
alias pi="ssh connermcd@pi"
alias py="python"
alias py2="python2"
alias tran="transmission-remote-cli"
# flags
alias aa="/home/connermcd/.tintin/tmuxit"
alias bd="cd $BLOG && bundle exec rake generate && bundle exec rake rsync && cd -"
alias calc="libreoffice --calc"
alias cleanvim="vim -N -u NONE"
alias draw="libreoffice --draw"
alias duh="du -h -d 0 [^.]*"
alias impress="libreoffice --impress"
alias inotify="echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p"
alias l="ls -al"
alias ls='ls --color=auto'
alias myip="curl http://myip.dnsomatic.com && echo ''"
alias pandoc="pandoc --latex-engine=lualatex"
alias pretty-json="python2 -mjson.tool"
alias print="lpr -P 'Deskjet_F4500_USB'"
alias screencast-external="ffmpeg -f alsa -ac 2 -i hw:1,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias screencast-internal="ffmpeg -f alsa -ac 2 -i hw:0,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias screencast-sys-out="ffmpeg -f alsa -ac 2 -i hw:0,1 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias syms="find . -maxdepth 1 -type l -print | while read line; do ls -alc "\$line"; done"
alias unneeded="sudo pacman -Rsn \$(pacman -Qqdt)"
alias webcast-external="ffmpeg -f alsa -ac 2 -i hw:1,0 -f v4l2 -itsoffset 1 -s 640x480 -i /dev/video0 -acodec pcm_s16le -vcodec libx264 -y output.mkv"
alias webcast-internal="ffmpeg -f alsa -ac 2 -i hw:0,0 -f v4l2 -itsoffset 1 -s 640x480 -i /dev/video0 -acodec pcm_s16le -vcodec libx264 -y output.mkv"
alias wifi="wicd-cli -y"
alias lswifi="wicd-cli -yx && wicd-cli -yS && wicd-cli -yl"
alias writer="libreoffice --writer"
# Functions {{{1
speedup() {
   </dev/null ffmpeg -i "$*" -filter atempo=1.5 "${*%%.mp3}-150.mp3"
}
nack() {
   vim -c "Nack $*"
}
t() {
   todo=/home/connermcd/Dropbox/Tech/todo/todo.sh
   if [ "$1" == "edit" ]; then
      $todo $1
   else
      $todo $* | less -FXRS
   fi
}
webrick() {
   port="${1:-3000}"
   ruby -r webrick -e "s = WEBrick::HTTPServer.new(:Port => $port, :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}
pacsize() {
   sudo pacman -Qi | \
      awk 'BEGIN{sort="sort -k2 -n"} /Name/ {name=$3} /Size/ {size=$4/1024;print name":",size,"Mb"|sort}' | \
      less
}
pget() {
   pirate-get -t "$*"
}
ptime() {
   find -type f -name \"*\" -print0 | \
      xargs -0  mplayer -vo dummy -ao dummy -identify 2>/dev/null | \
      perl -nle '/ID_LENGTH=([0-9\.]+)/ && (\$t +=\$1) && printf \"%02d:%02d:%02d\n\",\$t/3600,\$t/60%60,\$t%60' | \
      tail -n 1
}
youtube() {
   mplayer -fs -cookies -cookies-file cookie.txt $(youtube-dl -g --cookies cookie.txt -f 18 "http://www.youtube.com/watch?v=$1")
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
# Gnuplot {{{1
# cat ~/.bash_history | awk '/^git/ { print $1, $2 }' | sort | uniq -dc | sort | gnuplot -p -e 'set terminal x11; set xtics rotate 180; set key off; plot [:][:] "< cat -" using 1: xtic(3) with histogram' | feh -
