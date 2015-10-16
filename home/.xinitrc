#!/bin/bash

for _ in /etc/X11/xinit/xinitrc.d/*; do
  [[ -f "$_" ]] && . "$_"
done

systemctl --user import-environment PATH
systemctl --user start wm.target

# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit
echo "Starting at $(date)"
exec qtile
