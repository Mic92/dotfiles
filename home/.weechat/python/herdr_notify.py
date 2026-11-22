# herdr_notify.py - notify via Herdr on highlight/PM, but only if still unread
# after a delay (so messages you read in time stay quiet).
#
# In a herdr-core terminal (the Herdr app) weechat reports itself as blocked
# through OSC 7501, which makes the app alert and list it like an agent that
# needs attention; this also works when weechat runs on another machine. In
# the Herdr TUI it needs `herdr` in PATH.

import base64
import contextlib
import itertools
import os
from pathlib import Path

import weechat

SETTINGS: dict[str, str] = {
    "delay": "15",  # seconds; skip notification if read within this window
    "sound": "request",  # none | done | request
    "position": "top-right",  # top-left | top-right | bottom-left | bottom-right
    "max_body_length": "200",  # 0 = no limit
}

# Pending notifications keyed by id; the timer callback only receives a string,
# so we cannot pack pointers/text into it (null bytes break the C API).
_pending: dict[str, tuple[str, str, str]] = {}
_ids = itertools.count(1)
# Buffers we reported as needing attention and that are still unread.
_blocked: set[str] = set()


def report_status(state: str, title: str = "", body: str = "") -> None:
    fields = [f"state={state}", "app=weechat"]
    if state == "blocked":
        fields.append("kind=question")
    for key, text in (("title", title), ("msg", body)):
        if text:
            fields.append(f"{key}={base64.b64encode(text.encode()).decode()}")
    # weechat owns stdout; the controlling terminal is the safe way out.
    with contextlib.suppress(OSError), Path("/dev/tty").open("w") as tty:
        tty.write(f"\033]7501;{':'.join(fields)}\033\\")


def hotlist_changed_cb(data: str, signal: str, signal_data: str) -> int:
    if not _blocked:
        return weechat.WEECHAT_RC_OK
    _blocked.difference_update([b for b in _blocked if not buffer_is_unread(b)])
    if not _blocked:
        report_status("clear")
    return weechat.WEECHAT_RC_OK


def buffer_is_unread(buffer: str) -> bool:
    infolist = weechat.infolist_get("hotlist", "", "")
    unread = False
    while weechat.infolist_next(infolist):
        if weechat.infolist_pointer(infolist, "buffer_pointer") == buffer:
            unread = True
            break
    weechat.infolist_free(infolist)
    return unread


def notify_timer_cb(data: str, remaining_calls: str) -> int:
    entry = _pending.pop(data, None)
    if entry is None:
        return weechat.WEECHAT_RC_OK
    buffer, title, body = entry
    # A closed buffer drops out of the hotlist too, so this guards stale pointers.
    if not buffer_is_unread(buffer):
        return weechat.WEECHAT_RC_OK
    if os.environ.get("HC_TERMINAL_ID"):
        _blocked.add(buffer)
        report_status("blocked", title, body)
        return weechat.WEECHAT_RC_OK
    weechat.hook_process_hashtable(
        "herdr",
        {
            "arg1": "notification",
            "arg2": "show",
            "arg3": title,
            "arg4": "--body",
            "arg5": body,
            "arg6": "--position",
            "arg7": weechat.config_get_plugin("position"),
            "arg8": "--sound",
            "arg9": weechat.config_get_plugin("sound"),
        },
        10000,
        "",
        "",
    )
    return weechat.WEECHAT_RC_OK


def message_cb(
    data: str,
    buffer: str,
    date: str,
    tags: str,
    displayed: str,
    highlight: str,
    prefix: str,
    message: str,
) -> int:
    if not int(displayed):
        return weechat.WEECHAT_RC_OK
    is_private = "notify_private" in tags.split(",")
    if not (is_private or int(highlight)):
        return weechat.WEECHAT_RC_OK

    name = weechat.buffer_get_string(buffer, "short_name") or weechat.buffer_get_string(
        buffer, "name"
    )
    title = "PM from %s" % (prefix or name) if is_private else f"{prefix} in {name}"

    max_len = int(weechat.config_get_plugin("max_body_length"))
    body = (
        message
        if max_len <= 0 or len(message) <= max_len
        else message[: max_len - 1] + "\u2026"
    )

    key = str(next(_ids))
    _pending[key] = (buffer, title, body)
    delay = int(weechat.config_get_plugin("delay")) * 1000
    weechat.hook_timer(delay, 0, 1, "notify_timer_cb", key)
    return weechat.WEECHAT_RC_OK


if weechat.register(
    "herdr_notify",
    "Mic92",
    "0.1",
    "MIT",
    "Delayed notifications via Herdr, only if still unread",
    "",
    "",
):
    for option, default in SETTINGS.items():
        if not weechat.config_is_set_plugin(option):
            weechat.config_set_plugin(option, default)
    weechat.hook_print(
        "", "notify_message,notify_private,notify_highlight", "", 1, "message_cb", ""
    )
    weechat.hook_signal("hotlist_changed", "hotlist_changed_cb", "")
