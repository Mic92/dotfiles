# -*- coding: utf-8 -*-

import glob
import os
import subprocess

import color
import icons
from i3pystatus import Status
from phone_battery import PhoneBattery

status = Status(standalone=True)

status.register("clock", format="%a %-d %b %X KW%V", color=color.text_normal)
icons.nerdfont(status, "")

status.register("pulseaudio", format="{volume}")
icons.nerdfont(status, "")

status.register(
    "network",
    interface="wlp3s0",
    format_up="{essid} {quality_bar} {v4cidr} {bytes_sent}/s|{bytes_recv}/s",
    color_up=color.text_normal,
    color_down=color.text_down,
    dynamic_color=False,
)
icons.nerdfont(status, "")

bat_status = {"DIS": "-", "CHR": "+", "FULL": "↯"}
for bat_path in glob.glob("/sys/class/power_supply/BAT*"):
    bat = os.path.basename(bat_path)
    status.register(
        "battery",
        format="{status}{percentage:.2f}% [{glyph}] {remaining:%E%hh:%Mm}",
        alert=True,
        alert_percentage=10,
        battery_ident=bat,
        status=bat_status,
        color=color.text_normal,
        charging_color=color.text_normal,
        full_color=color.text_normal,
        critical_color=color.text_down,
        hints={"separator": False,
               "separator_block_width": 5} if bat == "BAT1" else {},
    )

icons.nerdfont(status, "")

status.register(PhoneBattery, interval=60)
icons.nerdfont(status, "", size="medium")

status.run()
