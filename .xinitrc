if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

wicd-client -t &

xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xmodmap ~/.Xmodmap
xbindkeys
xbacklight -set 50

xdotool key Num_Lock

eval $(cat ~/.fehbg)
emerald &
# conky &

/usr/lib/notification-daemon-1.0/notification-daemon &>/dev/null &
/usr/bin/notify-listener.py &>/dev/null &

#tint2 &

exec xmonad
#exec compiz ccp
