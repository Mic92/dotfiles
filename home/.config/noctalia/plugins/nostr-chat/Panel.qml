import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.Commons
import qs.Widgets

Item {
  id: root

  property var pluginApi: null
  property var chat: pluginApi?.mainInstance?.chat || null
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
  property real contentPreferredWidth: 500
  property real contentPreferredHeight: 460
  implicitWidth: contentPreferredWidth
  implicitHeight: contentPreferredHeight

  // Qt's MarkdownText renderer hardcodes link colour to the palette's
  // `link` role (classic web blue), which is unreadable on our dark
  // surface. Override it panel-wide.
  palette.link: Color.mSecondary

  // Relative-time formatter for the tiny timestamp under each bubble.
  // Absolute times would be noise for a chat that's mostly "just now".
  function ago(ts) {
    const s = Math.max(0, (Date.now() - ts) / 1000);
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
        pointSize: Style.fontSizeXL
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

          // Click to reply. TapHandler rather than MouseArea so it
          // coexists with text selection inside NText.
          TapHandler {
            acceptedButtons: Qt.LeftButton
            onTapped: {
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
            NText {
              id: msgText
              Layout.fillWidth: true
              text: modelData.text
              wrapMode: Text.Wrap
              textFormat: Text.MarkdownText
              color: row.mine ? Color.mOnPrimary : Color.mOnSurface
            }
            RowLayout {
              Layout.alignment: row.mine ? Qt.AlignRight : Qt.AlignLeft
              spacing: Style.marginXS
              NText {
                text: ago(modelData.ts)
                font.pixelSize: Style.fontSizeXXS
                color: row.mine
                  ? Qt.alpha(Color.mOnPrimary, 0.6)
                  : Color.mOnSurfaceVariant
              }
              // Peer acks with a kind-7 reaction once it's received
              // the DM — render whatever emoji it sent, or a check mark.
              NText {
                visible: (modelData.ack ?? "") !== ""
                text: modelData.ack === "+" ? "✓" : (modelData.ack ?? "")
                font.pixelSize: Style.fontSizeXXS
                color: row.mine
                  ? Qt.alpha(Color.mOnPrimary, 0.8)
                  : Color.mTertiary
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
        onClicked: filePicker.openFilePicker()
      }
      NIconButton {
        icon: "send"
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
