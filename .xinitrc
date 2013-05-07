if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# starts wireless manager
wicd-client -t &

# sets pointer so it's not just an X
xsetroot -cursor_name left_ptr

# reads X configuration
xrdb ~/.Xresources

# sets key mappings - have to do it twice for some reason
xmodmap ~/.Xmodmap
xmodmap ~/.Xmodmap

# sets custom key bindings
xbindkeys

# sets brightness to half
xbacklight -set 50

# turns num lock on automatically
xdotool key Num_Lock

# sets background
eval $(cat ~/.fehbg)

# activates libnotify
/usr/lib/notification-daemon-1.0/notification-daemon &>/dev/null &
/usr/bin/notify-listener.py &>/dev/null &

# xcompmgr -cfD 5 &
exec xmonad

# vim: ft=sh
