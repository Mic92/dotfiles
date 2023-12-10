# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import json
import os
import re
import shlex
import subprocess
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime

from libqtile import bar, hook, layout, widget
from libqtile.backend.wayland import InputConfig
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.core.manager import Qtile
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile.widget import base

mod = "mod4"
terminal = guess_terminal()


@lazy.function
def create_screenshot(qtile: Qtile) -> None:
    target = os.path.expanduser(f"~/upload/{datetime.now():%Y%m%d-%H%M%S}.png")
    qtile.cmd_spawn(f"grim {shlex.quote(target)}")
    qtile.cmd_spawn(f"wl-copy {shlex.quote(target)}")
    qtile.cmd_spawn(f'notify-send "Screenshot saved at {target}"')


keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        [mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key(
        [mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key(
        [mod],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen on the focused window",
    ),
    Key(
        [mod],
        "t",
        lazy.window.toggle_floating(),
        desc="Toggle floating on the focused window",
    ),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawn("rofi -show run"), desc="Spawn rofi"),
    Key(
        [mod],
        "Print",
        create_screenshot,
        desc="Take a screenshot of the focused window",
    ),
    Key([mod], "s", create_screenshot, desc="Take a screenshot of the focused window"),
    # screen key on framework
    Key([mod], "p", lazy.spawn("wlr-randr --output eDP-1 --on"), desc="Launch firefox"),

    Key(["control", "mod1"], "F1", lazy.core.change_vt(1), desc="Switch to VT 1"),
    Key(["control", "mod1"], "F2", lazy.core.change_vt(2), desc="Switch to VT 2"),
    Key(["control", "mod1"], "F3", lazy.core.change_vt(3), desc="Switch to VT 3"),
    Key(["control", "mod1"], "F4", lazy.core.change_vt(4), desc="Switch to VT 4"),
    Key(["control", "mod1"], "F5", lazy.core.change_vt(5), desc="Switch to VT 5"),
    Key(["control", "mod1"], "F6", lazy.core.change_vt(6), desc="Switch to VT 6"),
]

groups = [
    Group(
        "1:web",
        init=True,
        persist=False,
        position=1,
        layout="max",
        matches=[
            Match(
                wm_class=re.compile("chromium|firefox"),
            )
        ],
    ),
    Group(
        "2:dev",
        init=True,
        persist=False,
        position=2,
        layout="max",
        matches=[Match(wm_class="foot")],
    ),
    Group(
        "3:im",
        init=True,
        persist=False,
        position=3,
        layout="max",
        matches=[Match(wm_class=re.compile("ferdium|Signal"))],
    ),
]

for i in range(4, 10):
    groups.append(Group(str(i), init=True, persist=False, position=i))

for i, group in enumerate(groups):
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                str(i + 1),
                lazy.group[group.name].toscreen(),
                desc=f"Switch to group {group.name}",
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                str(i + 1),
                lazy.window.togroup(group.name, switch_group=True),
                desc=f"Switch to & move focused window to group {group.name}",
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="SauceCodePro Nerd Font Mono",
    fontsize=13,
    padding=3,
)
extension_defaults = widget_defaults.copy()

top_widgets = [
    widget.GroupBox(hide_unused=True),
    widget.Prompt(),
    widget.WindowTabs(),
    widget.StatusNotifier(),
    widget.Sep(),
    widget.TextBox("󰁹", fontsize=13),
    widget.Battery(charge_char="+", discharge_char="-", full_char="↯"),
    widget.Sep(),
    widget.TextBox("", name="default", fontsize=20),
    widget.PulseVolume(),
    widget.Sep(),
    widget.TextBox("", name="default", fontsize=20),
    widget.Wlan(interface="wlp170s0"),
    widget.Sep(),
    widget.Clock(format="%a %-d %b %T KW%V"),
]
cpu_graph = widget.CPUGraph(
    samples=50, line_width=1, width=50, graph_color="FF2020", fill_color="C01010"
)
memory_widget = widget.Memory(measure_mem="G")


@dataclass
class IOStat:
    device: str
    read: int
    write: int


class DiskIO(base.ThreadPoolText):
    defaults = [
        ("update_interval", 3, "update time in seconds"),
    ]
    last_stats: dict[str, IOStat] = {}
    last_update = datetime.now()

    def __init__(self, **config: dict[str, int | float | str]) -> None:
        base.ThreadPoolText.__init__(self, "", **config)
        self.add_defaults(DiskIO.defaults)

    def _configure(self, qtile: Qtile, bar: bar.Bar) -> None:
        base.ThreadPoolText._configure(self, qtile, bar)
        self.add_callbacks({"Button1": self.force_update})

    def parse_line(self, line: str) -> IOStat:
        cols = line.split()
        return IOStat(cols[2], int(cols[5]), int(cols[9]))

    def diskio(self) -> str:
        now = datetime.now()
        interval = max((now - self.last_update).seconds, 1)
        read = 0
        write = 0
        # Linux kernel documentation: Documentation/iostats.txt
        with open("/proc/diskstats") as f:
            for line in f:
                stat = self.parse_line(line)
                last_stat = self.last_stats.get(stat.device, stat)
                # Check for overflows and counter resets (> 2^32)
                if last_stat.read > stat.read or last_stat.write > stat.write:
                    last_stat = stat
                # Diskstats are absolute, substract our last reading
                # * divide by timediff because we don't know the timer value
                read += (stat.read - last_stat.read) / interval
                write += (stat.write - last_stat.write) / interval
                self.last_stats[stat.device] = stat
        self.last_update = now
        return f"R: {read/2048:.0f} MB/s W: {write/2048:.0f} MB/s"

    def poll(self) -> str:
        try:
            return self.diskio()
        except Exception as e:
            return f"Error: {e}"


class FPing(base.ThreadPoolText):
    defaults = [
        ("update_interval", 60, "update time in seconds"),
    ]

    def __init__(self, **config: dict[str, int | float | str]) -> None:
        base.ThreadPoolText.__init__(self, "", **config)
        self.add_defaults(FPing.defaults)

    def _configure(self, qtile: Qtile, bar: bar.Bar) -> None:
        base.ThreadPoolText._configure(self, qtile, bar)
        self.add_callbacks({"Button1": self.force_update})

    def parse_line(self, line: str) -> dict[str, str | int | float | None]:
        # fping -q -c2 1.1.1.1 _gateway 2606:4700:4700::1111 192.168.59.1
        # 1.1.1.1              : xmt/rcv/%loss = 2/2/0%, min/avg/max = 12.6/13.7/14.9
        # _gateway             : xmt/rcv/%loss = 2/2/0%, min/avg/max = 2.43/3.58/4.73
        # 2606:4700:4700::1111 : xmt/rcv/%loss = 2/0/100%
        # 192.168.59.1         : xmt/rcv/%loss = 2/0/100%
        m = re.match(
            r"([a-zA-Z0-9.:_]+)\s*: xmt/rcv/%loss = (\d+)/(\d+)/(\d+)%(?:, min/avg/max = (\d+\.\d+)/(\d+\.\d+)/(\d+\.\d+))?",
            line,
        )
        data: defaultdict[str, str | int | float | None] = defaultdict(lambda: None)
        if m is None:
            return data
        data["addr"] = m.group(1)
        data["xmt"] = int(m.group(2))
        data["rcv"] = int(m.group(3))
        data["loss"] = float(m.group(4))
        if m.group(5) is not None:
            data["min"] = float(m.group(5))
            data["avg"] = float(m.group(6))
            data["max"] = float(m.group(7))
        return data

    def format_target(self, data: dict[str, int | float | None]) -> str:
        if avg := data["avg"]:
            return f"{avg}ms"
        return "N/A"

    def fping(self) -> str:
        addrs = ["1.1.1.1", "_gateway", "2606:4700:4700::1111"]
        cmd = ["fping", "-q", "-c2", *addrs]

        process = subprocess.run(cmd, capture_output=True, text=True)

        stats = {}
        for line in process.stderr.split("\n"):
            if line == "":
                continue
            data = self.parse_line(line)
            stats[data["addr"]] = data
        msg = "v4: " + self.format_target(stats["1.1.1.1"])
        msg += " / "
        msg += "v6: " + self.format_target(stats["2606:4700:4700::1111"])
        if gw := stats.get("_gateway"):
            msg += " / "
            msg += "gw: " + self.format_target(gw)
        return msg

    def poll(self) -> str:
        try:
            return self.fping()
        except Exception as e:
            return f"Error: {e}"


class Net(widget.Net):
    def interface_get(self) -> list[str]:
        now = datetime.now()
        if self.cached_interfaces and (now - self.last_update).seconds < 120:
            return self.cached_interfaces
        proc = subprocess.run(["ip", "--json", "link"], check=True, capture_output=True)
        interfaces = json.loads(proc.stdout)
        self.cached_interfaces = []
        for interface in interfaces:
            name = interface["ifname"]
            if name.startswith(("e", "wl")):
                self.cached_interfaces.append(name)
        self.last_update = now
        return self.cached_interfaces

    def interface_set(self, _: list[str]):
        pass

    interface = property(interface_get, interface_set)
    cached_interfaces: None | list[str] = None
    last_update = datetime.now()


bottom_widgets = [
    widget.TextBox("", name="default", fontsize=17),
    cpu_graph,
    widget.Sep(),
    widget.TextBox("󰍛", name="default", fontsize=17),
    memory_widget,
    widget.Sep(),
    widget.TextBox("", name="default", fontsize=17),
    Net(format="{down:.0f}{down_suffix} ↓↑ {up:.0f}{up_suffix}"),
    widget.Sep(),
    widget.ThermalSensor(),
    widget.Sep(),
    widget.Load(),
    widget.Sep(),
    FPing(),
    widget.Sep(),
    DiskIO(),
]


screens = [
    Screen(
        top=bar.Bar(top_widgets, 24),
        # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
        # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        bottom=bar.Bar(bottom_widgets, 24),
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
    ),
]
screens.append(screens[0])

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True


wl_input_rules = {
    "type:touchpad": InputConfig(
        dwt=True, tap=True, natural_scroll=True, middle_emulation=True
    ),
    "type:keyboard": InputConfig(
        kb_options="ctrl:nocaps", kb_layout="us", kb_variant="altgr-intl"
    ),
}


def systemd_run(command: list[str]) -> list[str]:
    return ["systemd-run", "--collect", "--user", f"--unit={command[0]}", "--", *command]


@hook.subscribe.startup
def autostart():
    subprocess.run(
        [
            "systemctl",
            "--user",
            "import-environment",
            "WAYLAND_DISPLAY",
        ]
    )
    # fmt: off
    sway_lock = [
        "swaylock",
        "--screenshots",
        "--clock",
        "--indicator",
        "--indicator-radius", "100",
        "--indicator-thickness", "7",
        "--effect-blur", "7x5",
        "--effect-vignette", "0.5:0.5",
        "--ring-color", "bb00cc",
        "--key-hl-color", "880033",
        "--line-color", "00000000",
        "--inside-color", "00000088",
        "--separator-color", "00000000",
        "--grace", "10",
        "--fade-in", "0.2",
    ]
    commands = [
        ["firefox"],
        ["kanshi"],
        ["mako"],
        ["foot", "--server"],
        ["ferdium"],
        # start it twice because it doesn't create a window the first time
        ["signal-desktop"],
        [
            "swayidle",
            "-w",
            "timeout", "300", shlex.join(sway_lock),
            "before-sleep", shlex.join(sway_lock),
        ],
    ]
    # fmt: on
    for command in commands:
        subprocess.Popen(systemd_run(command))


# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
