#!/bin/bash

for _ in /etc/X11/xinit/xinitrc.d/*; do
  [[ -f $_ ]] && . "$_"
done

systemctl --user set-environment "DISPLAY=$DISPLAY"

xrdb -merge ~/.Xresources
xset -b +dpms 0 0 300
setxkbmap -option terminate:ctrl_alt_bksp
# capslock is escape
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
pulseaudio --start
netlink-notify &
evolution &

services=(app@firefox app@pidgin copyq dropbox)
for srv in "${services[@]}"; do
  systemctl --user --no-block start "$srv"
done

if awesome --version; then
  exec awesome 2>&1 >> ~/.config/awesome/log
else
  echo "Awesome WM is not working. Fix this before continue."
fi
