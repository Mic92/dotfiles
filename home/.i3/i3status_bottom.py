# -*- coding: utf-8 -*-

import multiprocessing
import os
import subprocess

import color
import icons
from i3pystatus import Status
from i3pystatus.disk import Disk

import yubikey

status = Status(standalone=True)

status.register(
    "mpd",
    format="{title} {status} {album} {song_elapsed}/{song_length}",
    status={"pause": "", "play": "▶", "stop": "◾"},
    color=color.text_normal,
)

status.register(
    "network",
    interface="enp0s25",
    format_up="{v4cidr} {bytes_sent}/s|{bytes_recv}/s",
    color_down=color.text_down,
    color_up=color.text_up,
    auto_units=True
)
icons.conkysymbol(status, "i")

status.register(
    "disk", path="/", format="{used}/{total}G [{avail}G]", color=color.text_normal
)

status.register(
    "mem",
    format="{used_mem}/{total_mem}MB",
    color=color.text_normal,
    warn_color=color.text_warn,
)
icons.conkysymbol(status, "J")

status.register("load", format="Load: {avg1} {avg5}", color=color.text_normal)

## Shows your CPU temperature, if you have a Intel CPU
status.register("temp", format="{temp:.0f}°C", color=color.text_normal)
icons.nerdfont(status, "")

count = multiprocessing.cpu_count()
fmt = ["{usage_cpu%d:02}%%" % cpu for cpu in range(count)]
status.register("cpu_usage", format="/".join(fmt), color=color.text_normal)
icons.nerdfont(status, "")

status.register(yubikey.YubiKeyTouchDetector, hints={"markup": "pango"})

status.run()
