import QtQuick
import QtQuick.Layouts
import qs.Commons
import qs.Widgets

// UI-only settings. Daemon config (peer pubkey, relays, blossom, rbw
// entry) lives in the home-manager module — it needs to be known at
// systemd unit start, before the shell is even running.
ColumnLayout {
  id: root
  property var pluginApi: null
  spacing: Style.marginM

  function s(k, d) { return pluginApi?.pluginSettings?.[k] ?? d; }
  function set(k, v) { pluginApi?.setPluginSetting?.(k, v); }

  NText {
    Layout.fillWidth: true
    text: "Peer configuration (pubkey, relays, display name) is managed by "
        + "the NixOS module — see services.nostr-chat in your config."
    wrapMode: Text.Wrap
    color: Color.mOnSurfaceVariant
  }

  NSpinBox {
    Layout.fillWidth: true
    label: "History limit"
    description: "Messages kept in the panel's in-memory mirror (sqlite keeps everything)."
    from: 20; to: 1000; stepSize: 20
    value: s("maxHistory", 200)
    onValueModified: set("maxHistory", value)
  }
}
