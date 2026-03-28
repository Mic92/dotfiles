import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Services.UI

Item {
    id: root

    property var pluginApi: null
    property var launcher: null
    property string name: "Internet"
    property bool handleSearch: false
    property string supportedLayouts: "list"
    property bool supportsAutoPaste: false

    // Connection table. Mirrors nixosModules/kde/internet-chooser.sh.
    // `up`:   list of nmcli connection names to bring up
    // `down`: list to bring down (|| true — they may already be inactive)
    // "default" just tears down the tether links so NM falls back to the
    // normal autoconnect profile (wired/wifi).
    readonly property var connections: ({
        "bluetooth": {
            up: ["Pixel 8 Network"],
            down: [],
            icon: "bluetooth",
            description: "Tether via phone (Bluetooth PAN)"
        },
        "phone-wifi": {
            up: ["the.network"],
            down: [],
            icon: "wifi",
            description: "Tether via phone hotspot"
        },
        "default": {
            up: [],
            down: ["the.network", "Pixel 8 Network"],
            icon: "arrow-back-up",
            description: "Drop tethers, return to autoconnect"
        }
    })

    Process {
        id: switchProc
        property string connLabel: ""
        stderr: StdioCollector {}
        onExited: (exitCode) => {
            if (exitCode !== 0) {
                const err = switchProc.stderr.text.trim() || "nmcli failed";
                Logger.e("InternetProvider", connLabel, "failed:", err);
                ToastService.showError("Network: " + err);
                return;
            }
            ToastService.showNotice("Network: " + connLabel + " active");
        }
    }

    function init() {
        Logger.i("InternetProvider", "init");
    }

    function handleCommand(searchText) {
        return searchText.startsWith(">net");
    }

    function commands() {
        return [{
            name: ">net",
            description: "Switch internet connection (tethering)",
            icon: "world",
            isTablerIcon: true,
            isImage: false,
            onActivate: function() {
                launcher.setSearchText(">net ");
            }
        }];
    }

    function getResults(searchText) {
        const trimmed = searchText.trim();
        if (!trimmed.startsWith(">net")) return [];

        const query = trimmed.slice(">net".length).trim().toLowerCase();
        const names = Object.keys(connections);

        const matched = query
            ? names.filter(n => n.toLowerCase().includes(query))
            : names;

        return matched.map(formatEntry);
    }

    function formatEntry(name) {
        const conn = connections[name];
        return {
            name: name,
            description: conn.description,
            icon: conn.icon,
            isTablerIcon: true,
            isImage: false,
            provider: root,
            onActivate: function() {
                root.switchTo(name);
                launcher.close();
            }
        };
    }

    // Quote a connection name for sh. nmcli names can contain spaces
    // ("Pixel 8 Network"), so wrap in single quotes and escape any
    // embedded ones the POSIX way.
    function shq(s) {
        return "'" + s.replace(/'/g, "'\\''") + "'";
    }

    function switchTo(name) {
        if (switchProc.running) {
            Logger.w("InternetProvider", "switch already running, ignoring");
            return;
        }
        const conn = connections[name];
        const lines = [];

        // Downs are best-effort: connection may already be inactive.
        for (const c of conn.down)
            lines.push("nmcli connection down " + shq(c) + " || true");

        // Ups must succeed — propagate failure so the toast shows the error.
        for (const c of conn.up)
            lines.push("nmcli connection up " + shq(c));

        // If there's nothing to up (the "default" case), the script exits 0
        // after the best-effort downs, which is exactly what we want.
        switchProc.connLabel = name;
        switchProc.command = ["sh", "-c", lines.join("\n")];
        switchProc.running = true;
    }
}
