# Setup {{{1
stty -ixon -ixoff # turns off CTRL-S
[[ $- != *i* ]] && return
bind '"\C-f":"cd_with_fzf\n"'
bind '"\C-o":"open_with_fzf\n"'
bind '"\C-v":"vim\n"'

export BROWSER=/usr/bin/qutebrowser
export EDITOR=vim
export FZF_DEFAULT_COMMAND="fd -H"
export PROMPT_COMMAND=__prompt_command

__prompt_command() {
   code=$?
   [[ $code != 0 ]] && echo -e "$RED✗ ${code}${RESET_COLOR}"
   PS1="$(ps1_hostname)\[\e[1;36m\]\W\[\e[1;31m\]:\[\e[0m\] "
}

ps1_hostname() {
   host=$(hostname)
   user=$(whoami)
   if [[ "$host" != "lemur" || "$user" != "connermcd" ]]; then
      echo "\[\e[1;30m\]$user\[\e[0;37m\]@\[\e[1;36m\]$host "
   fi
}

bind TAB:menu-complete
# less colors {{{2
export LESS_TERMCAP_mb=$'\e[01;31m' # begin blinking
export LESS_TERMCAP_md=$'\e[01;34m' # begin bold
export LESS_TERMCAP_me=$'\e[0m'     # end mode
export LESS_TERMCAP_se=$'\e[0m'     # end standout-mode
export LESS_TERMCAP_so=$'\e[01;32m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\e[0m'     # end underline
export LESS_TERMCAP_us=$'\e[01;36m' # begin underline
# Aliases {{{1
alias calc="libreoffice --calc"
alias cleanvim="/bin/vim -N -u NONE"
alias cp="rsync --archive --human-readable --progress --verbose --whole-file"
alias scp="rsync --archive --checksum --compress --human-readable --itemize-changes --rsh=ssh --stats --verbose"
alias draw="libreoffice --draw"
alias duh="du -h -d 0 [^.]*"
alias em="neomutt"
alias grep="grep --color=always"
alias gv="vim -c 'GV'"
alias htop="sudo htop"
alias impress="libreoffice --impress"
alias l="ls -al"
alias ls='ls --color=auto'
alias linode='ssh conner@linode'
alias m="vimpc"
alias myip="curl http://myip.dnsomatic.com && echo ''"
alias open="xdg-open"
alias pandoc="pandoc --pdf-engine=lualatex -H $HOME/.config/pandoc/fonts.tex"
alias pretty-json="python2 -mjson.tool"
alias print="lpr -P 'Deskjet_F4500'"
alias r='ranger'
# alias screencast="ffmpeg -f alsa -ac 2 -i loopout -f alsa -ac 2 -i hw:2,0 -filter_complex amix=inputs=2:duration=first -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec aac -vcodec libx264 -crf 0 -preset medium output.mp4"
alias screencast="ffmpeg -f pulse -i 3 -ac 2 -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y screencast.mkv"
alias screencast-no-sound="ffmpeg -f x11grab -r 30 -s 1920x1080 -i :0.0 -vcodec libx264 -preset ultrafast -crf 0 -y screencast.mkv"
alias slideshow="pandoc --pdf-engine=lualatex -H $HOME/.config/pandoc/fonts.tex -t beamer -o slideshow.pdf"
alias syms="find . -maxdepth 1 -type l -print | while read line; do ls -alc "\$line"; done"
alias tran="transmission-remote-cli"
alias vb="VBoxManage"
alias webcam="mplayer -noborder -tv driver=v4l2:gain=1:width=320:height=240:device=/dev/video0:fps=10:outfmt=rgb16 -geometry 100%:97% tv://"
alias webcast-external="ffmpeg -f alsa -ac 2 -i hw:1,0 -f v4l2 -itsoffset 1 -s 640x480 -i /dev/video0 -acodec pcm_s16le -vcodec libx264 -y output.mkv"
alias webcast-internal="ffmpeg -f alsa -ac 2 -i hw:0,0 -f v4l2 -itsoffset 1 -s 640x480 -i /dev/video0 -acodec pcm_s16le -vcodec libx264 -y output.mkv"
alias wifi="sudo wifi-menu -o"
alias writer="libreoffice --writer"
# Functions {{{1
open_with_fzf() {
# fd -t f -H | fzf -m --preview="xdg-mime query default {}" | xargs -ro -d "\n" xdg-open 2>&-
fd -t f -H | fzf -m --preview="xdg-mime query default {}" | xargs -ro -d "\n" xdg-open
}
cd_with_fzf() {
cd $HOME && cd "$(fd -t d | fzf --preview="tree -L 1 {}" --bind="space:toggle-preview" --preview-window=:hidden)" && clear
}
pacs() {
sudo pacman -Syy $(pacman -Ssq | fzf -m --preview="pacman -Si {}" --preview-window=:hidden --bind=space:toggle-preview)
}
compress-pdf() {
   gs -o "$2" -sDEVICE=pdfwrite -dPDFSETTINGS=/screen -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH "$1"
}
cut-video() {
   ffmpeg -i "$1" -ss "$2" -to "$3" -async 1 cut.mp4
}
concat-video() {
   ffmpeg -i "$1" -c copy -bsf:v h264_mp4toannexb -f mpegts intermediate1.ts
   ffmpeg -i "$2" -c copy -bsf:v h264_mp4toannexb -f mpegts intermediate2.ts
   ffmpeg -i "concat:intermediate1.ts|intermediate2.ts" -c copy -bsf:a aac_adtstoasc concat.mp4 && rm intermediate1.ts intermediate2.ts
}
pmid2bib() {
   curl -s "https://www.ncbi.nlm.nih.gov/pubmed/$1?report=xml&format=raw" | sed -e 's/&gt;/>/g' -e 's/&lt;/</g' | med2xml | xml2bib -nb -b >>bib.bib
}
pdf2bib() {
    pdftotext "$1" - | doigrep | while read doi; do doi2bib "$doi"; done
}
repair-pdf() {
   gs -o "$2" -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress "$1"
}
gong() {
   at "$1" today <<EOF
notify-send "Time to go"
mpc -q toggle
mplayer /usr/lib/libreoffice/share/gallery/sounds/gong.wav
EOF
}
pacman-purge() {
   sudo paccache -r
   sudo pacman -Rsn $(pacman -Qqdt)
}
pacsize() {
   sudo pacman -Qi | \
      awk 'BEGIN{sort="sort -k2 -n"} /Name/ {name=$3} /Size/ {size=$4/1024;print name":",size,"Mb"|sort}' | \
      less
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
speedup() {
   </dev/null ffmpeg -i "$*" -filter atempo=1.5 "${*%%.mp3}-150.mp3"
}
twitch() {
   INRES="1920x1080" # input resolution
   OUTRES="1280x720" # output resolution
   FPS="30" # target FPS
   # GOP="$(( $FPS * 2 ))" # i-frame interval, should be double of FPS
   GOP="60" # i-frame interval, should be double of FPS
   GOPMIN="$FPS" # min i-frame interval, should be equal to fps
   THREADS="4" # max 6
   CBR="1000k" # constant bitrate (should be between 1000k - 3000k)
   QUALITY="ultrafast"  # one of the many FFMPEG preset
   AUDIO_RATE="44100"
   STREAM_KEY="$(pass show web/twitch/key)"
   SERVER="live-dfw" # twitch server in frankfurt, see http://bashtech.net/twitch/ingest.php for list
   PROBESIZE="42M"

   ffmpeg \
      -f x11grab -s "$INRES" -r "$FPS" -probesize $PROBESIZE -i :0.0 \
      -f pulse -i 3 -f flv -ac 2 -ar $AUDIO_RATE \
      -vcodec libx264 -g $GOP -keyint_min $GOPMIN -b:v $CBR -minrate $CBR -maxrate $CBR -pix_fmt yuv420p\
      -s $OUTRES -preset $QUALITY -acodec libmp3lame -threads $THREADS -strict normal \
      -bufsize $CBR "rtmp://$SERVER.twitch.tv/app/$STREAM_KEY"
}
youtube-stream() {
   INRES="1920x1080" # input resolution
   OUTRES="1280x720" # output resolution
   FPS="30" # target FPS
   GOP="$(( $FPS * 2 ))" # i-frame interval, should be double of FPS
   GOPMIN="$FPS" # min i-frame interval, should be equal to fps
   THREADS="4" # max 6
   CBR="1000k" # constant bitrate (should be between 1000k - 3000k)
   QUALITY="ultrafast"  # one of the many FFMPEG preset
   AUDIO_RATE="44100"
   STREAM_KEY="$(pass show web/youtube/key)"
   PROBESIZE="42M"

   ffmpeg \
      -f x11grab -s "$INRES" -r "$FPS" -probesize $PROBESIZE -i :0.0 \
      -f pulse -i 3 -f flv -ac 2 -ar $AUDIO_RATE \
      -vcodec libx264 -g $GOP -keyint_min $GOPMIN -b:v $CBR -minrate $CBR -maxrate $CBR -pix_fmt yuv420p\
      -s $OUTRES -preset $QUALITY -acodec libmp3lame -threads $THREADS -strict normal \
      -bufsize $CBR "rtmp://a.rtmp.youtube.com/live2/$STREAM_KEY"
}
webrick() {
   port="${1:-3000}"
   ruby -r webrick -e "s = WEBrick::HTTPServer.new(:Port => $port, :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}
youtube() {
   mplayer -fs -cookies -cookies-file cookie.txt $(youtube-dl -g --cookies cookie.txt -f 18 "http://www.youtube.com/watch?v=$1")
}
syt() { pipe=`mktemp -u`; mkfifo -m 600 "$pipe" && for i in "$@"; do youtube-dl -qo "$pipe" "$i" & mplayer "$pipe" || break; done; rm -f "$pipe"; }
# Gnuplot {{{1
# cat ~/.bash_history | awk '/^git/ { print $1, $2 }' | sort | uniq -dc | sort | gnuplot -p -e 'set terminal x11; set xtics rotate 180; set key off; plot [:][:] "< cat -" using 1: xtic(3) with histogram' | feh -
