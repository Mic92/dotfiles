devices = [
    "turingmachine",
    "bernie",
    "phone",
]

hosts = {"turingmachine": "turingmachine.r", "bernie": "bernie.r"}

mac_addrs = {
    "headphone": "E8:07:BF:C8:6B:2C",
    "shannans_headphone": "00:6A:8E:60:0B:F7",
    "speaker": "E6:4D:D6:0A:CC:9B",
}


def tasker_rpc(cmd: str, arg: str) -> None:
    message = f"{cmd};{arg};"
    params = {"title": "Tasker", "message": message, "target": ["gt-i9195"]}
    hass.services.call("notify", "pushover", params, blocking=False)


def ssh_bluetooth(device: str, action: str, mac: str) -> None:
    host = hosts.get(device)
    if host is None:
        logger.warning(f"Unknown host: {host}")
        return
    hass.services.call(
        "shell_command",
        "ssh_bluetooth",
        {
            "host": host,
            "action": action,
            "mac": mac,
        },
        blocking=False,
    )


def connect(device: str, mac: str) -> None:
    if device == "phone":
        tasker_rpc("Connect Bluetooth", mac)
    else:
        ssh_bluetooth(device, "connect", mac)


def disconnect(device: str, mac: str) -> None:
    if device == "phone":
        tasker_rpc("Disconnect Bluetooth", mac)
    else:
        ssh_bluetooth(device, "disconnect", mac)


def main() -> None:
    action = data.get("action", None)

    device = ""
    if action == "connect":
        device = data.get("device", None)
        if not device:
            logger.warning("Called without 'device' parameter")
            return

    bluetooth_device = data.get("bluetooth_device", None)
    if not bluetooth_device:
        logger.warning("Called without 'bluetooth_device' parameter")
        return

    mac = mac_addrs.get(bluetooth_device)
    if not mac:
        logger.warning(
            "Called without unknown bluetooth device {bluetooth_device} parameter",
        )
        return

    for d in devices:
        if d != device:
            disconnect(d, mac)

    for d in devices:
        if d == device:
            connect(d, mac)


main()
