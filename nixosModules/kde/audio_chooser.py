import subprocess
import sys
from dataclasses import dataclass


@dataclass
class DeviceConfig:
    card: str
    profile: str
    bluetooth_addr: str | None = None


DEVICES: dict[str, DeviceConfig] = {
    "headphones": DeviceConfig(
        card="alsa_card.usb-Audeze_LLC_Audeze_Maxwell_PS_Dongle_0000000000000000-01",
        profile="output:analog-stereo+input:mono-fallback",
    ),
    "speakers": DeviceConfig(
        card="alsa_card.pci-0000_c1_00.6",
        profile="output:analog-stereo+input:analog-stereo",
    ),
    "headphones (output-only)": DeviceConfig(
        card="alsa_card.usb-Audeze_LLC_Audeze_Maxwell_PS_Dongle_0000000000000000-01",
        profile="output:analog-stereo",
    ),
    "earphones": DeviceConfig(
        card="bluez_output.50_C2_75_67_67_8C.1",
        profile="a2dp-sink",
        bluetooth_addr="50:C2:75:67:67:8C",
    ),
    "headphones (bt)": DeviceConfig(
        card="bluez_card.E0_49_ED_04_B7_B4",
        profile="a2dp-sink",
        bluetooth_addr="E0:49:ED:04:B7:B4",
    ),
    "soundbar": DeviceConfig(
        card="alsa_card.usb-DELL_DELL_Slim_Soundbar_SB522A_0-02",
        profile="output:analog-stereo",
    ),
}

WEBCAM: str = "alsa_card.usb-046d_C505e_HD_Webcam_C614D880-02"


def run_cmd(cmd: list[str], ignore_error: bool = True) -> bool:
    try:
        subprocess.run(cmd, check=True, capture_output=True)
    except subprocess.CalledProcessError:
        return not ignore_error
    else:
        return True


def notify(message: str) -> None:
    run_cmd(["notify-send", "-a", "audio-chooser", "Audio Chooser", message])


def show_menu() -> str | None:
    choices = list(DEVICES.keys())
    try:
        result = subprocess.run(
            ["fuzzel", "-d"],
            input="\n".join(["", *choices]),
            text=True,
            capture_output=True,
            check=True,
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return None


def set_audio_device(device_name: str) -> bool:
    if device_name not in DEVICES:
        notify(f"Invalid Option {device_name}")
        return False

    device: DeviceConfig = DEVICES[device_name]

    # Handle Bluetooth: disconnect all other BT devices, then connect if needed
    all_bt_addrs = {d.bluetooth_addr for d in DEVICES.values() if d.bluetooth_addr}

    for bt_addr in all_bt_addrs:
        action = "connect" if bt_addr == device.bluetooth_addr else "disconnect"
        run_cmd(["bluetoothctl", action, bt_addr])

    all_cards = [d.card for d in DEVICES.values()]
    for card in all_cards:
        if card == device.card:
            run_cmd(["pactl", "set-card-profile", device.card, device.profile])
        else:
            run_cmd(["pactl", "set-card-profile", card, "off"])

    notify(f"{device_name.title()} Connected")
    return True


def unmute_all() -> None:
    # Unmute all sinks
    try:
        result = subprocess.run(
            ["pactl", "list", "short", "sinks"],
            capture_output=True,
            text=True,
            check=True,
        )
        for line in result.stdout.strip().split("\n"):
            if line:
                sink_idx = line.split("\t")[0]
                run_cmd(["pamixer", "--unmute", "--sink", sink_idx])
    except subprocess.CalledProcessError:
        pass

    # Unmute all sources
    try:
        result = subprocess.run(
            ["pactl", "list", "short", "sources"],
            capture_output=True,
            text=True,
            check=True,
        )
        for line in result.stdout.strip().split("\n"):
            if line:
                source_idx = line.split("\t")[0]
                run_cmd(["pamixer", "--unmute", "--source", source_idx])
    except subprocess.CalledProcessError:
        pass


def main() -> None:
    # Always disable webcam mic
    run_cmd(["pactl", "set-card-profile", WEBCAM, "off"])

    selected: str | None = show_menu()
    if not selected:
        sys.exit(1)

    print(f"You Picked: {selected}")

    if not set_audio_device(selected):
        sys.exit(1)

    unmute_all()


if __name__ == "__main__":
    main()
