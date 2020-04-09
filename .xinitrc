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
setxkbmap -option ctrl:nocaps

# sets custom key bindings
xbindkeys

# sets brightness to half
xbacklight -set 50

# turns num lock on automatically
# xdotool key Num_Lock

# sets background
~/.fehbg &

sxhkd &

exec bspwm
# exec startplasma-x11
# exec i3 -V >> /var/log/i3/$(date +'%F-%k-%M-%S').log 2>&1

# vim: ft=sh
