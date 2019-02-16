# -*- coding: utf-8 -*-

import subprocess

import color
import icons
from i3pystatus import Status

status = Status(standalone=True)

status.register("clock", format="%a %-d %b %X KW%V", color=color.text_normal)
icons.nerdfont(status, "")

status.register("pulseaudio", format="{volume}")
icons.nerdfont(status, "")

status.register(
    "network",
    interface="wlan0",
    format_up="{essid} {quality_bar} {v4cidr}",
    color_up=color.text_normal,
    color_down=color.text_down,
)
icons.nerdfont(status, "")

bat_status = {"DIS": "-", "CHR": "+", "FULL": "↯"}
for bat in ["BAT0", "BAT1"]:
    status.register(
        "battery",
        format='{status}{percentage:.2f}% [{glyph}] {remaining:%E%hh:%Mm}',
        alert=True,
        alert_percentage=5,
        battery_ident=bat,
        status=bat_status,
        color=color.text_normal,
        charging_color=color.text_normal,
        full_color=color.text_normal,
        critical_color=color.text_down,
        hints={ "separator": False, "separator_block_width": 5 } if bat == "BAT1" else {},
    )
icons.nerdfont(status, "")

status.run()
