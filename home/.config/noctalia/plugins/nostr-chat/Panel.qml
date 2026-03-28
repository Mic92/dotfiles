import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.Commons
import qs.Widgets

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
  function snippet(text, n) {
    const t = (text || "").replace(/\s+/g, " ").trim();
    return t.length > n ? t.slice(0, n - 1) + "…" : t;
  }

  // SmartPanel.qml sizes by contentPreferred{Width,Height} — without
  // these it falls back to its 900px default, ignoring implicitHeight.
  property real contentPreferredWidth: 720
  property real contentPreferredHeight: 600
  implicitWidth: contentPreferredWidth
  implicitHeight: contentPreferredHeight

  // Qt's MarkdownText renderer hardcodes link colour to the palette's
  // `link` role (classic web blue), which is unreadable on our dark
  // surface. Override it panel-wide.
  palette.link: Color.mSecondary

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
      Rectangle {
        implicitWidth: 8; implicitHeight: 8; radius: 4
        color: chat?.streaming ? Color.mTertiary : Color.mError
      }
    }

    NDivider { Layout.fillWidth: true }

    // ── History ───────────────────────────────────────────────────────
    NListView {
      id: history
      Layout.fillWidth: true
      Layout.fillHeight: true
      model: chat?.messages ?? []
      clip: true
      spacing: Style.marginM
      // NListView's custom WheelHandler clamps contentY assuming
      // originY==0, which breaks once our reassigned-array model shifts
      // originY. 1.0 disables it and falls back to Qt's own scrolling.
      wheelScrollMultiplier: 1.0
      // The gradient fade looks wrong over chat bubbles — it's meant for
      // flat lists. Bubbles already provide their own visual boundary.
      showGradientMasks: false

      delegate: Item {
        id: row
        required property var modelData
        readonly property bool mine: modelData.from === "me"

        width: history.availableWidth
        implicitHeight: bubble.implicitHeight

        // Bubble floats left (bot) or right (me) at ~85% width so the
        // alignment itself reads as "who said this" without an avatar.
        Rectangle {
          id: bubble
          anchors.left:  row.mine ? undefined : parent.left
          anchors.right: row.mine ? parent.right : undefined
          // Image/quote bubbles snap to the 85% cap; plain text shrinks
          // to fit so short replies don't stretch edge-to-edge.
          width: ((modelData.image ?? "") !== "" || (modelData.replyTo ?? "") !== "")
            ? row.width * 0.85
            : Math.min(msgText.implicitWidth + Style.marginM * 2,
                       row.width * 0.85)
          implicitHeight: col.implicitHeight + Style.marginM * 2
          radius: Style.radiusS
          color: row.mine ? Color.mPrimary : Color.mSurfaceVariant
          // Subtle outline on bot bubbles so consecutive ones don't melt
          // into the panel background.
          border.width: row.mine ? 0 : 1
          border.color: Color.mOutline
          // Dim only while the outbox still owns it. Once a relay has
          // the wrap it's out of our hands — the single check mark
          // covers the rest, and a peer that never acks won't leave
          // the bubble looking half-finished forever.
          opacity: (row.mine && modelData.state === root.state.pending) ? 0.7 : 1.0
          Behavior on opacity { NumberAnimation { duration: 150 } }

          // Hover hint + cursor so tap-to-reply is discoverable.
          // HoverHandler/TapHandler rather than MouseArea so they
          // coexist with text selection inside NText.
          HoverHandler { id: hov; cursorShape: Qt.PointingHandCursor }
          Rectangle {
            anchors.fill: parent
            radius: parent.radius
            color: Color.mOnSurface
            opacity: hov.hovered ? 0.06 : 0
            Behavior on opacity { NumberAnimation { duration: 100 } }
          }
          TapHandler {
            acceptedButtons: Qt.LeftButton
            // Double-tap to reply — single-tap would fight text
            // selection. The hover cursor hints something's clickable.
            onDoubleTapped: {
              chat.replyTarget = { id: modelData.id, text: modelData.text };
              input.forceActiveFocus();
            }
          }

          ColumnLayout {
            id: col
            anchors.fill: parent
            anchors.margins: Style.marginM
            spacing: Style.marginXXS

            // Quoted snippet when this message is a threaded reply.
            // Resolved from the in-memory mirror; if the target has
            // scrolled out of maxHistory we just show the bar empty.
            Rectangle {
              visible: (modelData.replyTo ?? "") !== ""
              Layout.fillWidth: true
              implicitHeight: quote.implicitHeight + Style.marginXS * 2
              radius: Style.radiusXS
              color: row.mine
                ? Qt.alpha(Color.mOnPrimary, 0.15)
                : Qt.alpha(Color.mOnSurface, 0.08)
              // Click the quote to jump to the original. Linear scan
              // for the index is fine at maxHistory scale.
              TapHandler {
                onTapped: {
                  const arr = chat?.messages || [];
                  const idx = arr.findIndex(m => m.id === modelData.replyTo);
                  if (idx >= 0) history.positionViewAtIndex(idx, ListView.Center);
                }
              }
              NText {
                id: quote
                anchors.fill: parent
                anchors.margins: Style.marginXS
                text: {
                  const m = root.findMsg(modelData.replyTo);
                  return m ? "↳ " + root.snippet(m.text, 60) : "↳ (earlier message)";
                }
                font.pixelSize: Style.fontSizeXS
                elide: Text.ElideRight
                color: row.mine
                  ? Qt.alpha(Color.mOnPrimary, 0.7)
                  : Color.mOnSurfaceVariant
              }
            }

            // kind-15 attachments: daemon downloads + decrypts, then
            // pushes the local path. QML Image won't load a bare path
            // — needs the file:// scheme.
            Image {
              visible: (modelData.image ?? "") !== ""
              Layout.fillWidth: true
              Layout.preferredHeight: visible
                ? Math.min(implicitHeight * (width / Math.max(implicitWidth, 1)), 240)
                : 0
              source: modelData.image ? "file://" + modelData.image : ""
              fillMode: Image.PreserveAspectFit
              asynchronous: true
              cache: true
            }
            // TextEdit so code snippets and URLs are selectable.
            // readOnly keeps it display-only; selectByMouse enables
            // drag-select without stealing the double-tap-to-reply.
            TextEdit {
              id: msgText
              Layout.fillWidth: true
              text: modelData.text
              wrapMode: Text.Wrap
              textFormat: Text.MarkdownText
              readOnly: true
              selectByMouse: true
              selectionColor: Color.mPrimary
              selectedTextColor: Color.mOnPrimary
              color: row.mine ? Color.mOnPrimary : Color.mOnSurface
              font.family: Settings.data.ui.fontDefault
              font.pointSize: Style.fontSizeM * Settings.data.ui.fontDefaultScale
              font.weight: Style.fontWeightMedium
            }
            RowLayout {
              Layout.alignment: row.mine ? Qt.AlignRight : Qt.AlignLeft
              spacing: Style.marginXS
              NText {
                text: ago(modelData.ts)
                font.pixelSize: Style.fontSizeM
                color: row.mine
                  ? Qt.alpha(Color.mOnPrimary, 0.6)
                  : Color.mOnSurfaceVariant
              }
              // Delivery ladder: 🕓 pending → ✓ sent → ✓✓/emoji read.
              // ⚠ replaces the clock when retries pile up — tap for
              // retry-now, long-press to cancel. Peers that don't
              // send read receipts stop at ✓ and that's fine.
              NText {
                visible: row.mine
                text: {
                  if ((modelData.tries ?? 0) > 0) return "⚠";
                  if (modelData.state === root.state.pending) return "🕓";
                  const a = modelData.ack ?? "";
                  if (a === "") return "✓";
                  return (a === "+" || a === "✓") ? "✓✓" : a;
                }
                font.pixelSize: Style.fontSizeL
                color: (modelData.tries ?? 0) > 0
                  ? Color.mError
                  : Qt.alpha(Color.mOnPrimary, 0.8)
                TapHandler {
                  enabled: (modelData.tries ?? 0) > 0
                  onTapped: chat.retry(modelData.id)
                  onLongPressed: chat.cancel(modelData.id)
                }
              }
            }
          }
        }
      }

      onCountChanged: Qt.callLater(() => positionViewAtEnd())
    }

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
          text: root.snippet(chat?.replyTarget?.text ?? "", 80)
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

      NTextInput {
        id: input
        Layout.fillWidth: true
        placeholderText: chat?.streaming ? "Message " + root.peerName + "…" : "Waiting for daemon…"
        // Esc clears the reply target without reaching for the ×.
        Keys.onEscapePressed: if (chat?.replyTarget) chat.replyTarget = null
        onAccepted: {
          if (!chat) return;
          // Send regardless of streaming state — the daemon's outbox
          // queues it and retries when relays come back.
          chat.send(text);
          text = "";
        }
      }
      NIconButton {
        icon: "paperclip"
        tooltipText: "Attach image"
        // Match the input field height so the compose row doesn't look
        // like two different UI scales glued together.
        baseSize: input.height
        onClicked: filePicker.openFilePicker()
      }
      NIconButton {
        icon: "send"
        baseSize: input.height
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
}
