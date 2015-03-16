#!/bin/bash

for _ in /etc/X11/xinit/xinitrc.d/*; do
  [[ -f $_ ]] && . "$_"
done

systemctl --user import-environment DISPLAY DBUS_SESSION_BUS_ADDRESS
exec >&1 >> ~/.i3/log

xrdb -merge ~/.Xresources
xset -b +dpms 0 0 300
# Enable zapping (C-A-<Bksp> kills X)
setxkbmap -option terminate:ctrl_alt_bksp
# capslock is escape
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
pulseaudio --start

services=(chromium gajim copyq claws-mail)
for srv in "${services[@]}"; do
  systemctl --user start "$srv"
done

seafile-applet &

(
  sleep 4
  $HOME/bin/gajim-autojoin-slack.py
)&

## i3
# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit
echo "Starting at $(date)"
#exec i3 >> ~/.i3/log

## awesome
#if awesome --version; then
  export _JAVA_AWT_WM_NONREPARENTING=1
  exec awesome 2>&1 >> ~/.config/awesome/log
#else
#  echo "Awesome WM is not working. Fix this before continue."
#fi

#exec qtile
