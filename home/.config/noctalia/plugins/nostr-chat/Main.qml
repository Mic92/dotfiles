import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Services.UI

// Thin view layer. All state — history, dedup, outbox, reconnect —
// lives in nostr-chatd. We receive events via IpcHandler.recv() and
// send commands over a one-shot unix socket.
Item {
  id: root

  property var pluginApi: null
  property alias chat: chat

  function cfg(key) {
    const s = pluginApi?.pluginSettings || {};
    const d = pluginApi?.manifest?.metadata?.defaultSettings || {};
    return s[key] ?? d[key];
  }

  // XDG_RUNTIME_DIR is guaranteed by systemd-logind; without it rbw
  // (and thus the daemon) can't run either, so no fallback needed.
  // Quickshell.env returns QVariant — String() avoids "undefined/…".
  readonly property string sockPath:
    String(Quickshell.env("XDG_RUNTIME_DIR")) + "/nostr-chatd.sock"

  QtObject {
    id: chat
    property string myPubkey: ""
    property string peerName: ""   // from daemon's NOSTR_CHAT_DISPLAY_NAME
    property bool streaming: false
    property string lastError: ""
    property int unread: 0
    property var messages: []   // [{id, from, text, ts, ack, image, replyTo}]
    property var replyTarget: null  // {id, text} — set by Panel when user clicks a bubble

    function send(text) {
      if (!text.trim()) return;
      lastError = "";  // clear stale error on new attempt
      root.sockSend({
        cmd: "send", text: text,
        replyTo: replyTarget ? replyTarget.id : undefined,
      });
      replyTarget = null;
    }
    function sendFile(path) {
      if (!path) return;
      // NFilePicker returns bare paths; strip file:// just in case.
      if (path.startsWith("file://")) path = decodeURIComponent(path.slice(7));
      root.sockSend({ cmd: "send-file", path: path });
    }
  }

  // Open the panel idempotently — openPanel() is a no-op if already
  // showing, unlike togglePanel() which would slam it shut mid-read.
  function showPanel() {
    pluginApi?.withCurrentScreen(s => pluginApi.openPanel(s));
    sockSend({ cmd: "mark-read" });
  }

  // One-shot socket: connect → write → disconnect. The daemon reads
  // NDJSON lines and closes when we do.
  //
  // Quickshell's Socket fires connectedChanged for both edges and for
  // the *requested* state, not the actual one — writing inside the
  // handler races the real connect. Instead we write on the explicit
  // onConnectionStateChanged only when the underlying socket is open.
  Socket {
    id: sock
    path: root.sockPath
    property var queue: []
    property bool draining: false

    onConnectionStateChanged: {
      if (!connected || draining || queue.length === 0) return;
      draining = true;
      for (const c of queue) write(JSON.stringify(c) + "\n");
      flush();
      queue = [];
      // Let the daemon read before we hang up; Qt batches the close
      // otherwise and the writes never hit the wire.
      Qt.callLater(() => { connected = false; draining = false; });
    }
    onError: (e) => {
      chat.lastError = "daemon unreachable";
      chat.streaming = false;
      // Drop queued commands — they were one-shot anyway, and a stale
      // `send` firing minutes later when the daemon returns would be
      // surprising. The daemon's sqlite outbox handles real retries.
      queue = [];
      draining = false;
      Logger.w("NostrChat", "socket", e, "path", path);
    }
  }
  function sockSend(cmd) {
    sock.queue = sock.queue.concat([cmd]);
    if (!sock.connected) sock.connected = true;
    else sock.connectionStateChanged();  // already open — drain now
  }

  // Events pushed by the daemon via `quickshell ipc call ... recv <json>`.
  function recv(raw) {
    let ev;
    try { ev = JSON.parse(raw); }
    catch (e) { Logger.w("NostrChat", "bad ipc json", raw); return; }

    switch (ev.kind) {
    case "status": {
      chat.streaming = ev.streaming;
      chat.myPubkey = ev.pubkey || chat.myPubkey;
      chat.peerName = ev.name || chat.peerName;
      chat.unread = ev.unread || 0;
      // booted is set only on the daemon's very first status push —
      // distinguishes a fresh process (needs backfill) from the replay
      // handler's own status echo (would loop). Edge-detection on
      // streaming can't do this: nothing ever flips it false.
      if (ev.booted)
        sockSend({ cmd: "replay", n: cfg("maxHistory") || 50 });
      break;
    }

    case "msg": {
      const m = ev.msg;
      // Daemon dedups; we just keep a bounded in-memory mirror for the
      // ListView. Insert-sort by ts since replay + live can interleave.
      const entry = {
        id: m.id, text: m.content, ts: m.ts * 1000, ack: m.ack,
        image: m.image || "", replyTo: m.replyTo || "",
        from: m.dir === "out" ? "me" : "bot",
      };
      let arr = chat.messages.slice();
      let i = arr.length;
      while (i > 0 && arr[i-1].ts > entry.ts) i--;
      // Skip if already mirrored (replay after a live insert).
      if (arr.some(x => x.id === entry.id)) return;
      arr.splice(i, 0, entry);
      const max = cfg("maxHistory") || 200;
      if (arr.length > max) arr = arr.slice(-max);
      chat.messages = arr;

      // Auto-open on live bot replies. The daemon marks replayed
      // history as read, so shell startup won't pop the panel for
      // yesterday's conversation.
      if (m.dir === "in" && !m.read) root.showPanel();
      break;
    }

    case "ack": {
      const arr = chat.messages.slice();
      const i = arr.findIndex(x => x.id === ev.target);
      if (i >= 0) {
        arr[i] = Object.assign({}, arr[i], { ack: ev.mark });
        chat.messages = arr;
      }
      break;
    }

    case "img": {
      // Async download finished — patch the existing bubble in place.
      const arr = chat.messages.slice();
      const i = arr.findIndex(x => x.id === ev.target);
      if (i >= 0) {
        arr[i] = Object.assign({}, arr[i], { image: ev.image });
        chat.messages = arr;
      }
      break;
    }

    case "error":
      chat.lastError = ev.text;
      ToastService.showError((chat.peerName || "nostr-chat") + ": " + ev.text);
      break;
    }
  }

  property real _lastTap: 0
  IpcHandler {
    target: "plugin:nostr-chat"

    function recv(json: string) { root.recv(json); }

    function tap() {
      const now = Date.now();
      if (now - root._lastTap < 400) toggle();
      root._lastTap = now;
    }
    function toggle() {
      sockSend({ cmd: "mark-read" });
      pluginApi?.withCurrentScreen(s => pluginApi.togglePanel(s));
    }
    function send(text: string) { chat.send(text); }
  }

  // Ask the daemon to backfill on load. If it's not running yet the
  // socket write fails silently; the status handler above re-requests
  // when the daemon announces itself.
  Component.onCompleted: sockSend({ cmd: "replay", n: cfg("maxHistory") || 50 })
}
