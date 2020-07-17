# -*- coding: utf-8 -*-

import multiprocessing
import os
import subprocess

import color
import icons
from i3pystatus import Status
from i3pystatus.disk import Disk
from i3pystatus.now_playing import NowPlaying
from bitwarden import BitwardenPassword
from icinga import Icinga
from netdata import Netdata
from rhasspy import Rhasspy

import yubikey

status = Status(standalone=True)

ROOT = os.path.dirname(os.path.realpath(__file__))

status.register(
    "network",
    interface="enp0s20f0u1u4",
    format_up="{v4cidr} {bytes_sent}/s|{bytes_recv}/s",
    color_down=color.text_down,
    color_up=color.text_up,
    auto_units=True
)
icons.nerdfont(status, "ﯴ")

status.register("now_playing", format="{title} {song_elapsed}/{song_length} {status}")

status.register(
    "disk", path="/", format="{used}/{total}G [{avail}G]", color=color.text_normal
)

status.register(
    "mem",
    format="{used_mem}/{total_mem}MB",
    color=color.text_normal,
    warn_color=color.text_warn,
)
icons.nerdfont(status, "")

status.register("load", format="Load: {avg1} {avg5}", color=color.text_normal)

## Shows your CPU temperature, if you have a Intel CPU
status.register("temp", format="{temp:.0f}°C", color=color.text_normal)
icons.nerdfont(status, "")

count = multiprocessing.cpu_count()
fmt = ["{usage_cpu%d:02}%%" % cpu for cpu in range(count)]
status.register("cpu_usage", format="/".join(fmt), color=color.text_normal)
icons.nerdfont(status, "")

status.register("ping", host="1.1.1.1", color=color.text_normal)
status.register("ping", format="GW: {ping}ms", host="_gateway", color=color.text_normal)

#status.register(Icinga,
#                base_url="https://icingamaster.bsd.services:5665",
#                username="mic92-api",
#                password=BitwardenPassword("d1677bc8-1d2d-47c8-86ed-52132498e9c1"),
#                service_filter='match("eve.thalheim.io", host.name)',
#                ca_file=os.path.join(ROOT, "icingamaster-bsd-services-chain.pem"),
#                format="UP/DOWN: {ok}/{not_ok}",
#                interval=60,
#                color=color.text_normal,
#                warning_color=color.text_warn,
#                critical_color=color.text_down)

#status.register(Netdata,
#                base_url="https://netdata.thalheim.io",
#                color=color.text_normal,
#                warning_color=color.text_warn,
#                critical_color=color.text_down)

#status.register(yubikey.YubiKeyTouchDetector, hints={"markup": "pango"})

status.register(Rhasspy)

status.run()
