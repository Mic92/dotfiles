# -*- coding: utf-8 -*-

import multiprocessing
import os
import subprocess

import color
import icons
from filecount import FileCount
from i3pystatus import Status
from i3pystatus.disk import Disk

status = Status(standalone=True)

status.register("load", format="Load: {avg1} {avg5}", color=color.text_normal)

## Shows your CPU temperature, if you have a Intel CPU
status.register("temp", format="{temp:.0f}°C", color=color.text_normal)
icons.nerdfont(status, "")

count = multiprocessing.cpu_count()
fmt = ["{usage_cpu%d:02}%%" % cpu for cpu in range(count)]
status.register("cpu_usage", format="/".join(fmt), color=color.text_normal)
icons.nerdfont(status, "")

status.register("mem", format="{used_mem}/{total_mem}MB", color=color.text_normal)
icons.conkysymbol(status, "J")

status.register(
    FileCount,
    name="DHCP",
    format_up="{name}",
    path="/run/systemd/netif/leases/*",
    color_down=color.text_down
)

status.register(
    "network", interface="eth0", format_up="{v4cidr}", color_down=color.text_down
)
icons.conkysymbol(status, "i")

status.register(
    "disk", path="/", format="{used}/{total}G [{avail}G]", color=color.text_normal
)

status.register(
    "mpd",
    format="{title} {status} {album} {song_elapsed}/{song_length}",
    status={"pause": "", "play": "▶", "stop": "◾"},
    color=color.text_normal
)

status.run()
