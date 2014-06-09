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
export RED='\033[0;31m'
export YELLOW='\033[1;33m'
export GREEN='\033[0;32m'
export RESET_COLOR='\033[0m'

export BLOG="$HOME/Dropbox/Tech/src/rb/blog"
export BROWSER=/usr/bin/chromium
export EDITOR=vim
export HISTSIZE=10000
export PASSWORD_FILE="$HOME/Dropbox/Archive/Important/passwords"
export TIMEFORMAT="=> %E"
export ANDROID_HOME="/opt/android-sdks"
# Notes {{{2
export YEAR=2
export MODULE=9
export NOTES_DIR="$HOME/Dropbox/Archive/School/Med School/Notes"
# less colors {{{2
export LESS_TERMCAP_mb=$'\e[01;31m' # begin blinking
export LESS_TERMCAP_md=$'\e[01;34m' # begin bold
export LESS_TERMCAP_me=$'\e[0m'     # end mode
export LESS_TERMCAP_se=$'\e[0m'     # end standout-mode
export LESS_TERMCAP_so=$'\e[01;32m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\e[0m'     # end underline
export LESS_TERMCAP_us=$'\e[01;36m' # begin underline
# Aliases {{{1
alias aa="/home/connermcd/.tintin/tmuxit"
alias bd="cd $BLOG && bundle exec rake generate && bundle exec rake rsync && cd -"
alias calc="libreoffice --calc"
alias cleanvim="vim -N -u NONE"
alias cp="rsync --archive --human-readable --progress --verbose --whole-file"
alias scp="rsync --archive --checksum --compress --human-readable --itemize-changes --rsh=ssh --stats --verbose"
alias draw="libreoffice --draw"
alias duh="du -h -d 0 [^.]*"
alias em="mutt"
alias htop="sudo htop"
alias i="irssi"
alias impress="libreoffice --impress"
alias inotify="echo fs.inotify.max_user_watches=524288 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p"
alias l="ls -al"
alias linode="ssh connermcd@linode"
alias ls='ls --color=auto'
alias lswifi="wicd-cli -yx && wicd-cli -yS && wicd-cli -yl"
alias m="vimpc"
alias monitor="sudo monitor"
alias myip="curl http://myip.dnsomatic.com && echo ''"
alias open="xdg-open"
alias pandoc="pandoc --latex-engine=lualatex -H $HOME/.texlive/fonts.tex"
alias pi="ssh connermcd@pi"
alias pretty-json="python2 -mjson.tool"
alias print="lpr -P 'Deskjet_F4500_USB'"
alias py2="python2"
alias py="python"
alias r="ranger"
alias screencast-external="ffmpeg -f alsa -ac 2 -i hw:1,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias screencast-internal="ffmpeg -f alsa -ac 2 -i hw:0,0 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias screencast-sys-out="ffmpeg -f alsa -ac 2 -i hw:0,1 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv"
alias syms="find . -maxdepth 1 -type l -print | while read line; do ls -alc "\$line"; done"
alias t="/home/connermcd/Dropbox/Tech/todo/todo.sh"
alias tran="transmission-remote-cli"
alias unneeded="sudo pacman -Rsn \$(pacman -Qqdt)"
alias v="vim"
alias vb="VBoxManage"
alias webcast-external="ffmpeg -f alsa -ac 2 -i hw:1,0 -f v4l2 -itsoffset 1 -s 640x480 -i /dev/video0 -acodec pcm_s16le -vcodec libx264 -y output.mkv"
alias webcast-internal="ffmpeg -f alsa -ac 2 -i hw:0,0 -f v4l2 -itsoffset 1 -s 640x480 -i /dev/video0 -acodec pcm_s16le -vcodec libx264 -y output.mkv"
alias wifi="wicd-curses"
alias writer="libreoffice --writer"
# Functions {{{1
gong() {
   at "$1" today <<EOF
notify-send "Time to go"
mpc -q toggle
mplayer /usr/lib/libreoffice/share/gallery/sounds/gong.wav
EOF

}
pacsize() {
   sudo pacman -Qi | \
      awk 'BEGIN{sort="sort -k2 -n"} /Name/ {name=$3} /Size/ {size=$4/1024;print name":",size,"Mb"|sort}' | \
      less
}
pget() {
   pirate-get -t "$*"
}
poke() {
cat <<EOF | nc "$1" "$2"
HEAD / HTTP/1.1
Host: host
Connection: close

EOF
}
ptime() {
   find -type f -name \"*\" -print0 | \
      xargs -0  mplayer -vo dummy -ao dummy -identify 2>/dev/null | \
      perl -nle '/ID_LENGTH=([0-9\.]+)/ && (\$t +=\$1) && printf \"%02d:%02d:%02d\n\",\$t/3600,\$t/60%60,\$t%60' | \
      tail -n 1
}
recent() {
   find $HOME/Dropbox/ -type f -regex ".*\.\(md\|txt\)" -mtime -$1 -not -path "*dropbox*" -exec vim "{}" +
}
speedup() {
   </dev/null ffmpeg -i "$*" -filter atempo=1.5 "${*%%.mp3}-150.mp3"
}
textfiles() {
   file ${*:-*} | grep -E "text$" | sed 's/:.*//'
}
webrick() {
   port="${1:-3000}"
   ruby -r webrick -e "s = WEBrick::HTTPServer.new(:Port => $port, :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
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
pathadd $HOME/.gem/ruby/2.1.0/bin
pathadd $ANDROID_HOME/tools
pathadd $ANDROID_HOME/platform-tools
pathadd $ANDROID_HOME/build-tools/19.0.2
# }}} vim: fdm=marker
# Gnuplot {{{1
# cat ~/.bash_history | awk '/^git/ { print $1, $2 }' | sort | uniq -dc | sort | gnuplot -p -e 'set terminal x11; set xtics rotate 180; set key off; plot [:][:] "< cat -" using 1: xtic(3) with histogram' | feh -
