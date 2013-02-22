#!/usr/bin/env zsh

if [ -z "$DISPLAY" ] && [[ "$TTY" = "/dev/tty1" ]]; then
    trap logout INT
    startx -- vt$(fgconsole)

    echo "Logout in 10s: "
    for i in {1..10}; do
      echo -n "$i "
      sleep 1
    done

    logout
fi
