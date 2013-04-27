#!/bin/bash

exec dbus-launch bash -c '
    eval $(gnome-keyring-daemon --start)
    systemd --user
'
