import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Services.UI

Item {
    id: root

    property var pluginApi: null
    property var launcher: null
    property string name: "Bitwarden"
    property bool handleSearch: false
    property string supportedLayouts: "list"
    property bool supportsAutoPaste: false

    property int maxResults: 50
    property var database: []
    property bool loaded: false
    property bool loading: false
    property string loadError: ""

    // Second-level menu state: when set, show actions for this entry
    property var selectedEntry: null

    Process {
        id: unlockProc
        command: ["rbw", "unlock"]
        stderr: StdioCollector {}
        onExited: (exitCode) => {
            if (exitCode !== 0) {
                ToastService.showError("rbw unlock failed: " + unlockProc.stderr.text.trim());
            }
            root.fetchEntries();
        }
    }

    Process {
        id: listProc
        command: ["rbw", "list", "--fields", "id,name,user,folder"]
        stdout: StdioCollector {}
        stderr: StdioCollector {}
        onExited: (exitCode) => root.parseList(exitCode)
    }

    // Run rbw get/code and pipe the secret straight into wl-copy so it
    // never sits in a QML StdioCollector buffer. cliphist watches the
    // clipboard, so suppress its recording for this entry.
    Process {
        id: actionProc
        property string actionLabel: ""
        // "type" = auto-paste into focused window, "copy" = clipboard only
        property string mode: "type"
        stderr: StdioCollector {}
        onExited: (exitCode) => {
            if (exitCode !== 0) {
                const err = actionProc.stderr.text.trim() || "rbw command failed";
                Logger.e("RbwProvider", actionLabel, "failed:", err);
                ToastService.showError("rbw: " + err);
                return;
            }
            clearTimer.restart();
            if (actionProc.mode === "type") {
                typeDelay.pendingLabel = actionLabel;
                typeDelay.restart();
            } else {
                ToastService.showNotice("Copied " + actionLabel + " to clipboard");
            }
        }
    }

    Process {
        id: typeProc
        property string label: ""
        stdout: StdioCollector {}
        stderr: StdioCollector {}
        onExited: (exitCode) => {
            if (exitCode !== 0) {
                Logger.e("RbwProvider", "paste failed:", typeProc.stderr.text);
                ToastService.showError("wtype failed: " + typeProc.stderr.text.trim());
            } else {
                ToastService.showNotice("Typed " + typeProc.label);
            }
        }
    }

    Timer {
        id: typeDelay
        interval: 120
        repeat: false
        property string pendingLabel: ""
        onTriggered: {
            typeProc.label = pendingLabel;
            // niri bug #2314: wtype's virtual-keyboard protocol corrupts the
            // keymap when typing arbitrary text. Work around by simulating a
            // paste shortcut instead — the modifier combo doesn't trip the bug
            // in practice. The secret is already on the clipboard (placed by
            // runAction() or typeText()); we only send the key combo here so
            // the password never appears in argv.
            typeProc.command = ["wtype", "-M", "ctrl", "-M", "shift", "-k", "v", "-m", "shift", "-m", "ctrl"];
            typeProc.running = true;
        }
    }

    // "Auto-type" a literal string (username) by copying to clipboard and
    // simulating Ctrl+Shift+V after the launcher surface has fully closed.
    // Text is passed via stdin so it never shows up in ps/argv.
    Process {
        id: copyProc
        property string pendingLabel: ""
        command: ["wl-copy", "--sensitive"]
        stdinEnabled: true
        onStarted: {
            write(copyProc.pendingInput);
            copyProc.pendingInput = "";
            stdinEnabled = false; // close stdin -> wl-copy forks & exits
        }
        onExited: (exitCode) => {
            if (exitCode !== 0) {
                ToastService.showError("wl-copy failed");
                return;
            }
            clearTimer.restart();
            // Only start the paste timer once the clipboard is actually set,
            // avoiding a race where wtype fires before wl-copy finished.
            typeDelay.pendingLabel = copyProc.pendingLabel;
            typeDelay.restart();
        }
        property string pendingInput: ""
    }
    function typeText(text, label) {
        copyProc.pendingInput = text;
        copyProc.pendingLabel = label;
        copyProc.stdinEnabled = true;
        copyProc.running = true;
    }

    Timer {
        id: clearTimer
        // interval is set in init() once pluginApi is available
        interval: 45000
        repeat: false
        onTriggered: {
            // Best-effort: clear unconditionally to avoid leaking secrets.
            // wl-copy --sensitive already told cliphist to skip storing it.
            Quickshell.execDetached(["wl-copy", "--clear"]);
            Logger.i("RbwProvider", "Cleared clipboard after timeout");
        }
    }

    function init() {
        Logger.i("RbwProvider", "init");
        const secs = pluginApi?.pluginSettings?.clearAfterSeconds
                  ?? pluginApi?.manifest?.metadata?.defaultSettings?.clearAfterSeconds
                  ?? 45;
        clearTimer.interval = secs * 1000;
        fetchEntries();
    }

    function onOpened() {
        // Reset sub-menu selection. The vault list is cached from init();
        // re-fetching on every open is wasteful with ~1k entries.
        // Use the error-retry path if the user needs a manual refresh.
        selectedEntry = null;
    }

    function fetchEntries() {
        if (listProc.running) return;
        loading = true;
        loaded = false;
        loadError = "";
        listProc.running = true;
    }

    function parseList(exitCode) {
        loading = false;
        if (exitCode !== 0) {
            loadError = listProc.stderr.text.trim() || "rbw list failed";
            Logger.e("RbwProvider", "list failed:", loadError);
            if (launcher && launcher.activeProvider === root) launcher.updateResults();
            return;
        }
        const lines = listProc.stdout.text.split("\n");
        database = lines
            .filter(l => l.trim().length > 0)
            .map(l => {
                const [id, name, user, folder] = l.split("\t");
                return {
                    id: id,
                    name: name || "",
                    user: user || "",
                    folder: folder || "",
                    // Pre-compute a search key so fuzzy matching covers name + user
                    searchKey: [name, user, folder].filter(Boolean).join(" ")
                };
            });
        loaded = true;
        Logger.i("RbwProvider", "Loaded", database.length, "entries");
        if (launcher && launcher.activeProvider === root) launcher.updateResults();
    }

    function handleCommand(searchText) {
        return searchText.startsWith(">rbw");
    }

    function commands() {
        return [{
            name: ">rbw",
            description: "Search Bitwarden (rbw)",
            icon: "lock",
            isTablerIcon: true,
            isImage: false,
            onActivate: function() {
                launcher.setSearchText(">rbw ");
            }
        }];
    }

    function getResults(searchText) {
        const trimmed = searchText.trim();
        if (!trimmed.startsWith(">rbw")) return [];

        const query = trimmed.slice(">rbw".length).trim().toLowerCase();

        // Sub-menu: actions for a specific entry. Drop back to search if the
        // user starts typing so they aren't stuck in a stale sub-menu.
        if (selectedEntry) {
            if (query) {
                selectedEntry = null;
            } else {
                return entryActions(selectedEntry);
            }
        }

        if (loading) {
            return [{
                name: "Loading vault…",
                description: "Running rbw list",
                icon: "refresh",
                isTablerIcon: true,
                onActivate: function() {}
            }];
        }

        if (loadError) {
            return [{
                name: "rbw error",
                description: loadError + " — activate to retry / unlock",
                icon: "alert-circle",
                isTablerIcon: true,
                onActivate: function() {
                    // Chain unlock -> list so we don't re-list before pinentry finishes.
                    unlockProc.running = true;
                }
            }];
        }

        if (!loaded) return [];

        // Special command: manual vault refresh (new entries from rbw add/sync)
        if (query === "!") {
            return [{
                name: "Refresh vault",
                description: "Re-run rbw list to pick up new entries",
                icon: "refresh",
                isTablerIcon: true,
                onActivate: function() {
                    fetchEntries();
                    launcher.setSearchText(">rbw ");
                }
            }];
        }

        const entries = query
            ? FuzzySort.go(query, database, { limit: maxResults, key: "searchKey" }).map(r => r.obj)
            : database.slice(0, maxResults);

        return entries.map(formatEntry);
    }

    function formatEntry(entry) {
        const desc = [entry.user, entry.folder].filter(Boolean).join(" · ");
        return {
            name: entry.name,
            description: desc,
            icon: "key",
            isTablerIcon: true,
            isImage: false,
            provider: root,
            onActivate: function() {
                root.selectedEntry = entry;
                launcher.setSearchText(">rbw ");
                // Defer: calling updateResults synchronously from inside
                // activate() would mutate `results` while it's being iterated.
                Qt.callLater(() => launcher.updateResults());
            }
        };
    }

    function entryActions(entry) {
        function back() {
            root.selectedEntry = null;
            launcher.setSearchText(">rbw ");
            Qt.callLater(() => launcher.updateResults());
        }
        const title = entry.name + (entry.user ? " — " + entry.user : "");
        return [
            {
                name: "Type password",
                description: title,
                icon: "key",
                isTablerIcon: true,
                onActivate: function() {
                    runAction(["rbw", "get", entry.id], "password", "type");
                    launcher.close();
                }
            },
            {
                name: "Type username",
                description: entry.user || "(none)",
                icon: "user",
                isTablerIcon: true,
                onActivate: function() {
                    if (!entry.user) {
                        ToastService.showWarning("No username for " + entry.name);
                        return;
                    }
                    launcher.close();
                    Qt.callLater(() => root.typeText(entry.user, "username"));
                }
            },
            {
                name: "Type TOTP code",
                description: entry.name,
                icon: "clock",
                isTablerIcon: true,
                onActivate: function() {
                    runAction(["rbw", "code", entry.id], "TOTP code", "type");
                    launcher.close();
                }
            },
            {
                name: "Copy password",
                description: "To clipboard only (no auto-type)",
                icon: "clipboard",
                isTablerIcon: true,
                onActivate: function() {
                    runAction(["rbw", "get", entry.id], "password", "copy");
                    launcher.close();
                }
            },
            {
                name: "Back",
                description: "Return to search",
                icon: "arrow-left",
                isTablerIcon: true,
                onActivate: back
            }
        ];
    }

    function runAction(rbwCmd, label, mode) {
        if (actionProc.running) {
            Logger.w("RbwProvider", "Action already running, ignoring");
            return;
        }
        actionProc.actionLabel = label;
        actionProc.mode = mode || "type";
        // Pipe straight into wl-copy so the secret never touches QML memory.
        // --sensitive sets CLIPBOARD_STATE=sensitive so cliphist skips it.
        // pipefail: propagate rbw's exit so a locked vault or missing entry
        // surfaces as an error instead of silently copying empty output.
        // head -c -1: strip exactly the single trailing newline rbw emits
        // without mangling passwords that legitimately end in newlines.
        actionProc.command = [
            "sh", "-c",
            "set -o pipefail; "
            + rbwCmd.map(a => `'${a.replace(/'/g, "'\\''")}'`).join(" ")
            + " | head -c -1 | wl-copy --sensitive"
        ];
        actionProc.running = true;
    }
}
