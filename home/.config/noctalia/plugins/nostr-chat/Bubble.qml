import QtQuick
import QtQuick.Layouts
import Quickshell
import qs.Commons
import qs.Widgets
import "MsgText.js" as Txt

// One chat row: hover-reveal reply button + the bubble itself.
// Panel owns all state; this takes data in and emits intent out so the
// delegate stays dumb and testable.
Item {
  id: row

  // ── in ───────────────────────────────────────────────────────────
  // modelData because that's what ListView injects into delegate
  // roots; aliased to msg for readability.
  required property var modelData // {id, from, text, ts, ack, image, replyTo, state, tries}
  readonly property var msg: modelData
  property string pendingState: "pending"
  property string searchQuery: ""
  property bool   searchCurrent: false
  // Panel resolves replyTo → text (needs access to the full list).
  property string quotedText: ""
  // ago() depends on Panel's ticking clock, so it's injected.
  property var ago: ts => ""

  // ── out ──────────────────────────────────────────────────────────
  signal replyRequested
  signal jumpToQuote
  signal retryRequested
  signal cancelRequested

  readonly property bool mine: msg.from === "me"
  // Match locally — O(1) and can't drift from Panel's hit list since
  // it's the same predicate.
  readonly property bool searchHit:
    searchQuery !== "" && (msg.text || "").toLowerCase().includes(searchQuery)

  implicitHeight: bubble.implicitHeight

  // Hover-reveal reply button in the 15% gutter beside the bubble.
  // Lives on the row so it never covers text and doesn't fight
  // selectByMouse.
  NIconButton {
    icon: "corner-down-right"
    tooltipText: "Reply"
    baseSize: Style.baseWidgetSize * 0.7
    anchors.verticalCenter: bubble.verticalCenter
    anchors.left:  row.mine ? undefined : bubble.right
    anchors.right: row.mine ? bubble.left : undefined
    anchors.margins: Style.marginXS
    opacity: (hov.hovered || hovering) ? 1 : 0
    visible: opacity > 0
    Behavior on opacity { NumberAnimation { duration: 100 } }
    onClicked: row.replyRequested()
  }

  // Bubble floats left (peer) or right (me) at ~85% width so the
  // alignment itself reads as "who said this" without an avatar.
  Rectangle {
    id: bubble
    anchors.left:  row.mine ? undefined : parent.left
    anchors.right: row.mine ? parent.right : undefined
    // Image/quote bubbles snap to the cap; plain text shrinks to fit
    // so short replies don't stretch edge-to-edge.
    width: ((msg.image ?? "") !== "" || (msg.replyTo ?? "") !== "")
      ? row.width * 0.85
      : Math.min(msgText.implicitWidth + Style.marginM * 2, row.width * 0.85)
    implicitHeight: col.implicitHeight + Style.marginM * 2
    radius: Style.radiusS
    color: row.mine ? Color.mPrimary : Color.mSurfaceVariant
    border.width: row.searchHit ? 2 : (row.mine ? 0 : 1)
    border.color: row.searchHit
      ? (row.searchCurrent ? Color.mTertiary : Color.mSecondary)
      : Color.mOutline
    // Dim only while the outbox still owns it. Once a relay has the
    // wrap it's out of our hands — ✓ covers the rest.
    opacity: (row.mine && msg.state === row.pendingState) ? 0.7 : 1.0
    Behavior on opacity { NumberAnimation { duration: 150 } }

    HoverHandler { id: hov }

    ColumnLayout {
      id: col
      anchors.fill: parent
      anchors.margins: Style.marginM
      spacing: Style.marginXXS

      // Quoted snippet for threaded replies. Panel resolves the text;
      // we just render.
      Rectangle {
        visible: (msg.replyTo ?? "") !== ""
        Layout.fillWidth: true
        implicitHeight: quote.implicitHeight + Style.marginXS * 2
        radius: Style.radiusXS
        color: row.mine
          ? Qt.alpha(Color.mOnPrimary, 0.15)
          : Qt.alpha(Color.mOnSurface, 0.08)
        TapHandler { onTapped: row.jumpToQuote() }
        NText {
          id: quote
          anchors.fill: parent
          anchors.margins: Style.marginXS
          text: row.quotedText
            ? "↳ " + Txt.snippet(row.quotedText, 60)
            : "↳ (earlier message)"
          font.pixelSize: Style.fontSizeXS
          elide: Text.ElideRight
          color: row.mine
            ? Qt.alpha(Color.mOnPrimary, 0.7)
            : Color.mOnSurfaceVariant
        }
      }

      // kind-15 attachments: daemon downloads + decrypts, then pushes
      // the local path. QML Image won't load a bare path — needs the
      // file:// scheme.
      Image {
        visible: (msg.image ?? "") !== ""
        Layout.fillWidth: true
        Layout.preferredHeight: visible
          ? Math.min(implicitHeight * (width / Math.max(implicitWidth, 1)), 240)
          : 0
        source: msg.image ? "file://" + msg.image : ""
        fillMode: Image.PreserveAspectFit
        asynchronous: true
        cache: true
        // Open full-size — 240px is fine for a glance but useless for
        // reading a screenshot.
        TapHandler {
          grabPermissions: PointerHandler.TakeOverForbidden
          onTapped: Quickshell.execDetached(["xdg-open", msg.image])
        }
        HoverHandler { cursorShape: Qt.PointingHandCursor }
      }

      TextEdit {
        id: msgText
        Layout.fillWidth: true
        readonly property color linkColor:
          row.mine ? Color.mOnPrimary : Color.mSecondary
        text: Txt.colorizeLinks(
          row.searchHit
            ? Txt.highlight(msg.text, row.searchQuery,
                            Color.mTertiary, Color.mOnTertiary)
            : msg.text,
          linkColor)
        wrapMode: Text.Wrap
        textFormat: Text.MarkdownText
        readOnly: true
        selectByMouse: true
        // Own bubbles sit on mPrimary, so invert the selection palette
        // or the highlight vanishes.
        selectionColor: row.mine ? Color.mOnPrimary : Color.mPrimary
        selectedTextColor: row.mine ? Color.mPrimary : Color.mOnPrimary
        color: row.mine ? Color.mOnPrimary : Color.mOnSurface
        font.family: Settings.data.ui.fontDefault
        font.pointSize: Style.fontSizeM * Settings.data.ui.fontDefaultScale
        font.weight: Style.fontWeightMedium
        onLinkActivated: url => Quickshell.execDetached(["xdg-open", url])
        HoverHandler {
          cursorShape: msgText.hoveredLink !== ""
            ? Qt.PointingHandCursor : Qt.IBeamCursor
        }
      }

      RowLayout {
        Layout.alignment: row.mine ? Qt.AlignRight : Qt.AlignLeft
        spacing: Style.marginXS
        NText {
          text: row.ago(msg.ts)
          font.pixelSize: Style.fontSizeM
          color: row.mine
            ? Qt.alpha(Color.mOnPrimary, 0.6)
            : Color.mOnSurfaceVariant
        }
        // Delivery ladder: 🕓 pending → ✓ sent → ✓✓/emoji read.
        // ⚠ when retries pile up — tap to force, long-press to cancel.
        NText {
          visible: row.mine
          text: {
            if ((msg.tries ?? 0) > 0) return "⚠";
            if (msg.state === row.pendingState) return "🕓";
            const a = msg.ack ?? "";
            if (a === "") return "✓";
            return (a === "+" || a === "✓") ? "✓✓" : a;
          }
          font.pixelSize: Style.fontSizeL
          color: (msg.tries ?? 0) > 0
            ? Color.mError
            : Qt.alpha(Color.mOnPrimary, 0.8)
          TapHandler {
            enabled: (msg.tries ?? 0) > 0
            onTapped: row.retryRequested()
            onLongPressed: row.cancelRequested()
          }
        }
      }
    }
  }
}
