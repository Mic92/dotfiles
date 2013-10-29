#!/bin/bash

for _ in /etc/X11/xinit/xinitrc.d/*; do
  [[ -f $_ ]] && . "$_"
done

systemctl --user set-environment "DISPLAY=$DISPLAY"

xrdb -merge ~/.Xresources
xset -b +dpms 0 0 300
synclient TouchpadOff=1
setxkbmap -option terminate:ctrl_alt_bksp

services=(app@firefox app@thunderbird app@gajim copyq dropbox trayer xfce-notifyd)
for srv in "${services[@]}"; do
  systemctl --user --no-block start "$srv"
done

exec xmonad
