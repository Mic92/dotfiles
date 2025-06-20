# Todoman configuration
# https://todoman.readthedocs.io/en/stable/configure.html

from typing import Any

import click

# Path to calendar directories synced by vdirsyncer
path = "~/.local/share/calendars/*"

# Default list for new todos (must match a calendar name)
default_list = "personal"

# Date format
date_format = "%Y-%m-%d"

# Time format
time_format = "%H:%M"

# Datetime format
dt_separator = " "

# Default priority for new todos (1-9, where 1 is highest)
default_priority = 5

# Default due date offset for new todos (e.g., "3d" for 3 days)
# default_due = "1d"

# Start of the week (0 = Monday, 6 = Sunday)
startofweek = 0

# Color output
color = "auto"

# Monkeypatch click colors for Solarized Light
# This overrides the hardcoded bright colors in todoman
original_style = click.style


def solarized_style(text: str, **kwargs: Any) -> str:
    # Map todoman's bright colors to Solarized Light-friendly colors
    color_map = {
        "magenta": "bright_black",  # Priority markers - use base01
        "red": "red",  # Overdue - keep red but less bright
        "yellow": "magenta",  # Due soon - use magenta for visibility
        "white": "bright_black",  # Normal dates - use dark gray instead of white
    }

    if "fg" in kwargs and kwargs["fg"] in color_map:
        kwargs["fg"] = color_map[kwargs["fg"]]

    # Remove color for normal text
    if kwargs.get("fg") is None:
        kwargs.pop("fg", None)

    return original_style(text, **kwargs)


click.style = solarized_style

# Show completed todos by default
show_completed = False

# Humanize dates (show "tomorrow" instead of date)
humanize = True
