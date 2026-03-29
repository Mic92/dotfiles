import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Widgets
import "MsgText.js" as Txt

Item {
  id: root

  property var pluginApi: null
  property var chat: pluginApi?.mainInstance?.chat || null
  readonly property var state: pluginApi?.mainInstance?.state || ({})
  // Daemon pushes this in the status event — single source of truth is
  // the hm-module's displayName option, not a separate plugin setting.
  readonly property string peerName: chat?.peerName || "Chat"

  // Look up a message by id to render the quoted snippet above a
  // threaded reply. Linear scan is fine — maxHistory caps it at ~200.
  function findMsg(id) {
    const arr = chat?.messages || [];
    for (let i = arr.length - 1; i >= 0; i--)
      if (arr[i].id === id) return arr[i];
    return null;
  }


  // SmartPanel.qml sizes by contentPreferred{Width,Height} — without
  // these it falls back to its 900px default, ignoring implicitHeight.
  property real contentPreferredWidth: 720
  property real contentPreferredHeight: 600
  implicitWidth: contentPreferredWidth
  implicitHeight: contentPreferredHeight

  // Relative-time formatter for the tiny timestamp under each bubble.
  // Absolute times would be noise for a chat that's mostly "just now".
  // `_now` ticks every 30s so `ago()` bindings re-evaluate — otherwise
  // "now" freezes at send time and never becomes "1m".
  property real _now: Date.now()
  Timer { interval: 30000; running: root.visible; repeat: true; onTriggered: root._now = Date.now() }

  function ago(ts) {
    const s = Math.max(0, (_now - ts) / 1000);
    if (s < 60)   return "now";
    if (s < 3600) return Math.floor(s/60) + "m";
    if (s < 86400) return Math.floor(s/3600) + "h";
    return Qt.formatDateTime(new Date(ts), "ddd HH:mm");
  }

  ColumnLayout {
    anchors.fill: parent
    anchors.margins: Style.marginL
    spacing: Style.marginM

    // ── Header ────────────────────────────────────────────────────────
    RowLayout {
      Layout.fillWidth: true
      spacing: Style.marginS

      NIcon {
        icon: "message-chatbot"
        pointSize: Style.fontSizeXL * 1.4
        color: Color.mPrimary
      }
      ColumnLayout {
        spacing: 0
        Layout.fillWidth: true
        NText {
          text: root.peerName
          font.pixelSize: Style.fontSizeL
          font.bold: true
        }
        NText {
          text: chat?.streaming ? "connected" : "daemon offline"
          font.pixelSize: Style.fontSizeXS
          color: Color.mOnSurfaceVariant
        }
      }
      NIconButton {
        icon: "search"
        tooltipText: "Search"
        baseSize: Style.baseWidgetSize * 0.9
        onClicked: { searchBar.visible = !searchBar.visible; if (searchBar.visible) searchField.forceActiveFocus(); }
      }
      Rectangle {
        implicitWidth: 8; implicitHeight: 8; radius: 4
        color: chat?.streaming ? Color.mTertiary : Color.mError
      }
    }

    // ── Search ────────────────────────────────────────────────────────────
    // Case-insensitive substring match over the in-memory mirror.
    // hits[] indexes history.model (already newest-first), cursor walks
    // them. Closing clears the query so bubbles drop the outline.
    RowLayout {
      id: searchBar
      visible: false
      Layout.fillWidth: true
      spacing: Style.marginS

      // Store message IDs, not model indices — the reversed model
      // shifts by one every time a message arrives, which would point
      // every cached index at the wrong bubble.
      property var hits: []      // [id, id, …] newest-first
      property string current: ""
      readonly property string query: searchField.text.toLowerCase()
      onVisibleChanged: if (!visible) { searchField.text = ""; history.contentY = 0; }

      function refresh() {
        if (!query) { hits = []; current = ""; return; }
        const out = [];
        for (const m of history.model)
          if ((m.text || "").toLowerCase().includes(query)) out.push(m.id);
        hits = out;
        current = out[0] || "";
        jump();
      }
      function step(d) {
        if (!hits.length) return;
        const i = Math.max(0, hits.indexOf(current));
        current = hits[(i + d + hits.length) % hits.length];
        jump();
      }
      function jump() {
        if (!current) return;
        const i = history.model.findIndex(m => m.id === current);
        if (i >= 0) history.positionViewAtIndex(i, ListView.Center);
      }
      // Re-scan when messages arrive mid-search so the counter stays
      // honest. current is an ID so the cursor survives the refresh.
      readonly property int _watch: history.count
      on_WatchChanged: if (visible && query) {
        const keep = current;
        refresh();
        if (hits.includes(keep)) { current = keep; jump(); }
      }

      NTextInput {
        id: searchField
        Layout.fillWidth: true
        placeholderText: "Search messages…"
        inputItem.onTextChanged: searchBar.refresh()
        inputItem.Keys.onReturnPressed: e => searchBar.step(e.modifiers & Qt.ShiftModifier ? -1 : 1)
        inputItem.Keys.onEscapePressed: searchBar.visible = false
        function forceActiveFocus() { inputItem.forceActiveFocus(); }
      }
      NText {
        text: searchBar.hits.length
          ? (searchBar.hits.indexOf(searchBar.current) + 1) + "/" + searchBar.hits.length
          : (searchField.text ? "0" : "")
        color: Color.mOnSurfaceVariant
        font.pixelSize: Style.fontSizeS
      }
      NIconButton { icon: "chevron-up";   baseSize: Style.baseWidgetSize * 0.8; onClicked: searchBar.step(1) }
      NIconButton { icon: "chevron-down"; baseSize: Style.baseWidgetSize * 0.8; onClicked: searchBar.step(-1) }
      NIconButton { icon: "x";            baseSize: Style.baseWidgetSize * 0.8; onClicked: searchBar.visible = false }
    }

    NDivider { Layout.fillWidth: true }

    // ── History ───────────────────────────────────────────────────────
    // Wrapped so the "new messages" pill can float over the list
    // without joining the ColumnLayout flow.
    Item {
      Layout.fillWidth: true
      Layout.fillHeight: true

    NListView {
      id: history
      anchors.fill: parent
      // BottomToTop + reversed model: index 0 = newest = visual bottom.
      // This makes "stay at bottom" equivalent to contentY≈0, which
      // ListView preserves across our array-reassignment updates
      // without any positionViewAtEnd() gymnastics. Scrolling up
      // increases contentY into history as usual.
      verticalLayoutDirection: ListView.BottomToTop
      model: (chat?.messages ?? []).slice().reverse()
      clip: true
      // "Near bottom" = within two bubble-heights of contentY 0.
      readonly property bool atBottom: contentY < Style.baseWidgetSize * 2
      property int unseen: 0
      onAtBottomChanged: if (atBottom) unseen = 0
      spacing: Style.marginM
      // NListView's custom WheelHandler clamps contentY assuming
      // originY==0, which breaks once our reassigned-array model shifts
      // originY. 1.0 disables it and falls back to Qt's own scrolling.
      wheelScrollMultiplier: 1.0
      // The gradient fade looks wrong over chat bubbles — it's meant for
      // flat lists. Bubbles already provide their own visual boundary.
      showGradientMasks: false

      // ListView injects modelData into the delegate root; Bubble
      // declares that as required and aliases it to msg internally.
      delegate: Bubble {
        width: history.availableWidth
        pendingState: root.state.pending
        searchQuery: searchBar.visible ? searchBar.query : ""
        searchCurrent: searchBar.current === modelData.id
        quotedText: root.findMsg(modelData.replyTo)?.text ?? ""
        ago: root.ago

        onReplyRequested: {
          chat.replyTarget = { id: modelData.id, text: modelData.text };
          input.forceActiveFocus();
        }
        onJumpToQuote: {
          const i = history.model.findIndex(m => m.id === modelData.replyTo);
          if (i >= 0) history.positionViewAtIndex(i, ListView.Center);
        }
        onRetryRequested:  chat.retry(modelData.id)
        onCancelRequested: chat.cancel(modelData.id)
      }

      // BottomToTop keeps contentY stable on append, so the only
      // bookkeeping left is the unread pill. Our own sends always
      // snap — not seeing your message appear is worse than losing
      // scrollback.
      property int _lastCount: 0
      onModelChanged: {
        if (count <= _lastCount) { _lastCount = count; return; }
        _lastCount = count;
        if (model[0]?.from === "me") contentY = 0;
        else if (!atBottom) unseen++;
      }
    }

    // Floating "N new ↓" pill. Appears only when scrolled up and
    // messages arrived; tapping it jumps to the end and clears itself
    // via the atBottom watcher.
    Rectangle {
      visible: history.unseen > 0
      anchors.bottom: parent.bottom
      anchors.horizontalCenter: parent.horizontalCenter
      anchors.bottomMargin: Style.marginM
      radius: height / 2
      color: Color.mPrimary
      implicitWidth: pillRow.implicitWidth + Style.marginL * 2
      implicitHeight: pillRow.implicitHeight + Style.marginS * 2
      RowLayout {
        id: pillRow
        anchors.centerIn: parent
        spacing: Style.marginXS
        NText {
          text: history.unseen + " new"
          color: Color.mOnPrimary
          font.pixelSize: Style.fontSizeS
          font.bold: true
        }
        NIcon { icon: "chevron-down"; color: Color.mOnPrimary }
      }
      TapHandler { onTapped: history.contentY = 0 }
      HoverHandler { cursorShape: Qt.PointingHandCursor }
    }
    } // history wrapper

    // ── Compose ───────────────────────────────────────────────────────
    // Reply context bar — shown when a bubble was tapped. Cleared on
    // send (Main.qml) or by the × here.
    Rectangle {
      visible: (chat?.replyTarget ?? null) !== null
      Layout.fillWidth: true
      implicitHeight: replyRow.implicitHeight + Style.marginS * 2
      radius: Style.radiusS
      color: Color.mSurfaceVariant
      RowLayout {
        id: replyRow
        anchors.fill: parent
        anchors.margins: Style.marginS
        spacing: Style.marginS
        NIcon { icon: "corner-down-right"; color: Color.mPrimary }
        NText {
          Layout.fillWidth: true
          text: Txt.snippet(chat?.replyTarget?.text ?? "", 80)
          elide: Text.ElideRight
          font.pixelSize: Style.fontSizeS
          color: Color.mOnSurfaceVariant
        }
        NIconButton {
          icon: "x"
          baseSize: Style.baseWidgetSize * 0.7
          onClicked: chat.replyTarget = null
        }
      }
    }

    RowLayout {
      Layout.fillWidth: true
      spacing: Style.marginS

      // Custom multiline compose box — NTextInput wraps a single-line
      // TextField, but Nostr DMs routinely carry code snippets and
      // pasted logs. TextArea gives us newlines; we intercept Return
      // so plain Enter still sends (chat-app convention) while
      // Shift+Enter inserts a break.
      Control {
        id: input
        Layout.fillWidth: true
        // Grow with content up to ~5 lines, then scroll. Min matches
        // the icon buttons so the row stays aligned when empty.
        // TextArea.implicitHeight already includes its own padding.
        Layout.preferredHeight: Math.min(
          Math.max(inputArea.implicitHeight,
                   Style.baseWidgetSize * 1.1 * Style.uiScaleRatio),
          Style.baseWidgetSize * 4 * Style.uiScaleRatio)

        property alias text: inputArea.text
        signal accepted

        function forceActiveFocus() { inputArea.forceActiveFocus(); }

        onAccepted: {
          if (!chat) return;
          // Send regardless of streaming state — the daemon's outbox
          // queues it and retries when relays come back.
          chat.send(inputArea.text);
          inputArea.clear();
        }

        background: Rectangle {
          radius: Style.iRadiusM
          color: Color.mSurface
          border.color: inputArea.activeFocus ? Color.mSecondary : Color.mOutline
          border.width: Style.borderS
          Behavior on border.color { ColorAnimation { duration: Style.animationFast } }
        }

        contentItem: ScrollView {
          clip: true
          ScrollBar.horizontal.policy: ScrollBar.AlwaysOff
          TextArea {
            id: inputArea
            placeholderText: chat?.streaming ? "Message " + root.peerName + "…" : "Waiting for daemon…"
            placeholderTextColor: Qt.alpha(Color.mOnSurfaceVariant, 0.6)
            color: Color.mOnSurface
            wrapMode: TextEdit.Wrap
            selectByMouse: true
            background: null
            topPadding: Style.marginS
            bottomPadding: Style.marginS
            leftPadding: Style.marginM
            rightPadding: Style.marginM
            font.family: Settings.data.ui.fontDefault
            font.pointSize: Style.fontSizeS * Style.uiScaleRatio

            // Esc clears the reply target without reaching for the ×.
            Keys.onEscapePressed: if (chat?.replyTarget) chat.replyTarget = null

            // Ctrl+V: if the clipboard holds an image, dump it to
            // $XDG_RUNTIME_DIR and hand the path to the daemon with
            // unlink=true (same path as the screenshot keybind). Text
            // falls through to TextArea's own paste. canPaste reflects
            // text/plain availability, so it doubles as the "is this
            // an image?" probe without a wl-paste roundtrip.
            Keys.onPressed: e => {
              if (e.matches(StandardKey.Paste) && !canPaste) {
                e.accepted = true;
                pasteImage.running = true;
                return;
              }
              handleReturn(e);
            }

            // Enter sends, Shift+Enter newlines. Split out so the
            // paste interceptor above can share Keys.onPressed.
            function handleReturn(event) {
              if (event.key !== Qt.Key_Return && event.key !== Qt.Key_Enter) return;
              if ((event.modifiers & Qt.ShiftModifier)
                  && !(event.modifiers & Qt.ControlModifier)) {
                event.accepted = false;  // Shift+Enter → newline
              } else {
                event.accepted = true;
                if (text.trim().length > 0) input.accepted();
              }
            }
          }
        }
      }
      NIconButton {
        icon: "paperclip"
        tooltipText: "Attach image"
        // Fixed size + bottom-align — the input now grows with
        // multiline content and we don't want 4×-tall buttons.
        baseSize: Style.baseWidgetSize * 1.1 * Style.uiScaleRatio
        Layout.alignment: Qt.AlignBottom
        onClicked: filePicker.openFilePicker()
      }
      NIconButton {
        icon: "send"
        baseSize: Style.baseWidgetSize * 1.1 * Style.uiScaleRatio
        Layout.alignment: Qt.AlignBottom
        enabled: input.text.trim().length > 0
        onClicked: input.accepted()
      }
    }

    // Noctalia's in-panel Popup file picker — QtQuick.Dialogs.FileDialog
    // spawns a regular toplevel that layer-shell either occludes or
    // orphans. This one renders inside the panel overlay.
    NFilePicker {
      id: filePicker
      title: "Attach image"
      selectionMode: "files"
      nameFilters: ["*.png", "*.jpg", "*.jpeg", "*.gif", "*.webp"]
      initialPath: Quickshell.env("HOME") + "/Pictures"
      onAccepted: paths => { if (paths.length > 0) chat?.sendFile(paths[0]); }
    }

    Process {
      id: pasteImage
      property string tmp: ""
      command: ["sh", "-c",
        `f="$XDG_RUNTIME_DIR/nostr-chat-paste-$$"; ` +
        `wl-paste --type image > "$f" && printf %s "$f"`]
      stdout: StdioCollector { onStreamFinished: pasteImage.tmp = text }
      onExited: (code) => { if (code === 0 && tmp) chat?.sendFile(tmp, true); tmp = ""; }
    }

    // Drag-and-drop from file managers. Most offer text/uri-list; take
    // the first local file and let the daemon reject non-images.
    DropArea {
      parent: root  // cover the whole panel, not just this layout slot
      anchors.fill: parent
      onDropped: d => {
        if (!d.hasUrls) return;
        const u = d.urls[0].toString();
        if (u.startsWith("file://")) chat?.sendFile(decodeURIComponent(u.slice(7)));
      }
    }

    NText {
      visible: (chat?.lastError ?? "") !== ""
      text: chat?.lastError ?? ""
      color: Color.mError
      font.pixelSize: Style.fontSizeS
      Layout.fillWidth: true
      wrapMode: Text.Wrap
    }
  }

  onVisibleChanged: if (visible) Qt.callLater(() => input.forceActiveFocus())

  // Ctrl+F from anywhere in the panel. Shortcut rather than Keys so it
  // fires regardless of which TextArea currently has focus.
  Shortcut {
    sequence: StandardKey.Find
    enabled: root.visible
    onActivated: { searchBar.visible = true; searchField.forceActiveFocus(); }
  }
}
