
import multiprocessing
import os

import color
import icons
from i3pystatus import Status
from prometheus import Prometheus
from rhasspy import Rhasspy

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
status.register("ping", format="GW: {ping}ms", format_down="GW: N/A", host="_gateway", color=color.text_normal)
status.register("ping", format="V6: {ping}ms", format_down="V6: N/A", host="2606:4700:4700::1111", color=color.text_normal)

status.register(Prometheus,
                base_url="http://prometheus.r",
                color=color.text_normal,
                warning_color=color.text_warn,
                critical_color=color.text_down,
                interval=30)

#status.register(yubikey.YubiKeyTouchDetector, hints={"markup": "pango"})

status.register(Rhasspy)

status.run()
