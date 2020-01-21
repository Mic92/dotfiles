#!/usr/bin/env python3
import os
import subprocess
import urllib.request


def disable_sound(action: str) -> None:
    if action in ["down", "pre-down"]:
        return

    found = False
    for key, val in os.environ.items():
        if key.startswith("IP6_ADDRESS_"):
            if val.startswith("2001:630:3c1:90"):
                found = True
                break
            cmd = ["machinectl", "shell", "--uid=1000",
                   ".host", "amixer", "set", "Master", "mute"]
            if found:
                subprocess.run(cmd)


def set_geo_ip(action: str) -> None:
    if action in ["down", "pre-down"]:
        return

    with urllib.request.urlopen("https://ipapi.co/timezone") as response:
        timezone = response.read().decode("utf-8")
        print(f"Set timezone {timezone}")
        subprocess.run(["timedatectl", "set-timezone", timezone])


def main() -> None:
    action = os.environ.get("NM_DISPATCHER_ACTION", "unknown")
    hooks = [disable_sound, set_geo_ip]
    for hook in hooks:
        try:
            print(f"run hook {hook.__name__}")
            hook(action)
        except OSError as e:
            print(f"hook {hook.__name__} failed with {e}")


if __name__ == '__main__':
    main()
