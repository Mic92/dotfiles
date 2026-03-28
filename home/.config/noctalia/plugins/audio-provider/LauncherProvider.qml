import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Services.UI

Item {
    id: root

    property var pluginApi: null
    property var launcher: null
    property string name: "Audio"
    property bool handleSearch: false
    property string supportedLayouts: "list"
    property bool supportsAutoPaste: false

    // Device table. Mirrors nixosModules/kde/audio_chooser.py — keep in sync.
    // Each entry: { card, profile, bluetoothAddr? }
    // The plugin disables every other card so only the selected one routes
    // audio; that avoids pulseaudio's "helpful" automatic port switching.
    readonly property var devices: ({
        "audeze": {
            card: "alsa_card.usb-Audeze_LLC_Audeze_Maxwell_PS_Dongle_0000000000000000-01",
            profile: "output:analog-stereo+input:mono-fallback",
            icon: "headphones"
        },
        "speakers": {
            card: "alsa_card.pci-0000_c1_00.6",
            profile: "output:analog-stereo+input:analog-stereo",
            icon: "volume"
        },
        "audeze (output-only)": {
            card: "alsa_card.usb-Audeze_LLC_Audeze_Maxwell_PS_Dongle_0000000000000000-01",
            profile: "output:analog-stereo",
            icon: "headphones"
        },
        "earphones": {
            card: "bluez_output.50_C2_75_67_67_8C.1",
            profile: "a2dp-sink",
            bluetoothAddr: "50:C2:75:67:67:8C",
            icon: "bluetooth"
        },
        "audeze (bt)": {
            card: "bluez_card.E0_49_ED_04_B7_B4",
            profile: "a2dp-sink",
            bluetoothAddr: "E0:49:ED:04:B7:B4",
            icon: "bluetooth"
        },
        "beats": {
            card: "bluez_card.0C_53_B7_B5_B2_49",
            profile: "a2dp-sink",
            bluetoothAddr: "0C:53:B7:B5:B2:49",
            icon: "bluetooth"
        }
    })

    // Always-disabled card (webcam mic picks up too much noise).
    readonly property string webcamCard: "alsa_card.usb-046d_C505e_HD_Webcam_C614D880-02"

    Process {
        id: switchProc
        property string deviceLabel: ""
        stderr: StdioCollector {}
        onExited: (exitCode) => {
            if (exitCode !== 0) {
                const err = switchProc.stderr.text.trim() || "audio switch failed";
                Logger.e("AudioProvider", deviceLabel, "failed:", err);
                ToastService.showError("Audio: " + err);
                return;
            }
            ToastService.showNotice(deviceLabel + " connected");
        }
    }

    function init() {
        Logger.i("AudioProvider", "init");
    }

    function handleCommand(searchText) {
        return searchText.startsWith(">audio");
    }

    function commands() {
        return [{
            name: ">audio",
            description: "Switch audio output device",
            icon: "volume",
            isTablerIcon: true,
            isImage: false,
            onActivate: function() {
                launcher.setSearchText(">audio ");
            }
        }];
    }

    function getResults(searchText) {
        const trimmed = searchText.trim();
        if (!trimmed.startsWith(">audio")) return [];

        const query = trimmed.slice(">audio".length).trim().toLowerCase();
        const names = Object.keys(devices);

        const matched = query
            ? names.filter(n => n.toLowerCase().includes(query))
            : names;

        return matched.map(formatEntry);
    }

    function formatEntry(name) {
        const dev = devices[name];
        return {
            name: name,
            description: dev.card,
            icon: dev.icon || "volume",
            isTablerIcon: true,
            isImage: false,
            provider: root,
            onActivate: function() {
                root.switchTo(name);
                launcher.close();
            }
        };
    }

    // Build a single shell script that:
    //   1. disables the webcam mic
    //   2. connects/disconnects bluetooth as needed
    //   3. sets the chosen card's profile, turns all others off
    //   4. unmutes every sink/source
    // Running it as one sh -c keeps the Process bookkeeping trivial and
    // lets pactl failures on absent cards slide (bluetooth devices vanish
    // when disconnected, so set-card-profile off on them is expected to
    // fail — hence no set -e and `|| true` sprinkled in).
    function switchTo(name) {
        if (switchProc.running) {
            Logger.w("AudioProvider", "switch already running, ignoring");
            return;
        }
        const target = devices[name];

        // Dedup via object keys — avoids Set/spread which noctalia's QML
        // codebase never uses (keeps V4 engine compat unambiguous).
        const btSeen = {}, cardSeen = {};
        const allBtAddrs = [], allCards = [];
        for (const d of Object.values(devices)) {
            if (d.bluetoothAddr && !btSeen[d.bluetoothAddr]) {
                btSeen[d.bluetoothAddr] = true;
                allBtAddrs.push(d.bluetoothAddr);
            }
            if (!cardSeen[d.card]) {
                cardSeen[d.card] = true;
                allCards.push(d.card);
            }
        }

        const lines = [];
        lines.push(`pactl set-card-profile '${webcamCard}' off || true`);

        for (const addr of allBtAddrs) {
            const action = (addr === target.bluetoothAddr) ? "connect" : "disconnect";
            lines.push(`bluetoothctl ${action} '${addr}' || true`);
        }

        for (const card of allCards) {
            if (card === target.card) {
                lines.push(`pactl set-card-profile '${card}' '${target.profile}'`);
            } else {
                lines.push(`pactl set-card-profile '${card}' off || true`);
            }
        }

        // Unmute everything once the right profile is active.
        lines.push("pactl list short sinks   | cut -f1 | xargs -r -n1 pamixer --unmute --sink");
        lines.push("pactl list short sources | cut -f1 | xargs -r -n1 pamixer --unmute --source");

        switchProc.deviceLabel = name;
        switchProc.command = ["sh", "-c", lines.join("\n")];
        switchProc.running = true;
    }
}
