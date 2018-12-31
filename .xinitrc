if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# sets pointer so that it's not just an X
xsetroot -cursor_name left_ptr

# reads X configuration
xrdb $HOME/.Xresources

# sets key mappings - have to do it twice for some reason
xmodmap $HOME/.Xmodmap
xmodmap $HOME/.Xmodmap

# sets custom key bindings
xbindkeys

# sets brightness to half
xbacklight -set 50

# turns num lock on automatically
# xdotool key Num_Lock

# sets background
nitrogen --restore &

# activates libnotify
# /usr/lib/notification-daemon-1.0/notification-daemon &>/dev/null &
# /usr/bin/notify-listener.py &>/dev/null &

exec i3
#exec cinnamon
# exec i3 -V >> /var/log/i3/$(date +'%F-%k-%M-%S').log 2>&1

# vim: ft=sh
