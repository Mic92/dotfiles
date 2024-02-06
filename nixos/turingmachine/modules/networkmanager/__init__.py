#!/usr/bin/env python3
import hashlib
import logging
import logging.handlers
import os
import subprocess
import urllib.request

logger = logging.getLogger(__name__)
logger.addHandler(logging.handlers.SysLogHandler(address="/dev/log"))


def set_geo_ip(action: str) -> None:
    if action in ["down", "pre-down"]:
        return

    with urllib.request.urlopen("https://ipapi.co/timezone") as response:
        timezone = response.read().decode("utf-8")
        logger.info(f"Set timezone {timezone}")
        subprocess.run(["timedatectl", "set-timezone", timezone], check=False)


def assign_ula_ip(action: str) -> None:
    # DEVICE_IFACE
    iface = os.environ.get("DEVICE_IFACE", None)
    if iface is None:
        msg = f"No DEVICE_IFACE set for {action}"
        raise OSError(msg)
    if action in ["down", "pre-down"]:
        return

    hashsum = hashlib.sha256()
    hashsum.update(iface.encode("utf-8"))
    digest = hashsum.hexdigest()
    address = f"fd42:4492:6a6d:43:2:{digest[0:4]}:{digest[4:8]}:{digest[8:12]}/64"
    cmd = ["ip", "addr", "add", address, "dev", iface]
    subprocess.run(cmd, check=False)


def main() -> None:
    action = os.environ.get("NM_DISPATCHER_ACTION", "unknown")
    hooks = [assign_ula_ip, set_geo_ip]
    for hook in hooks:
        try:
            logger.info(f"run hook {hook.__name__}")
            hook(action)
        except OSError as e:
            logger.warning(f"hook {hook.__name__} failed with {e}")


if __name__ == "__main__":
    main()
