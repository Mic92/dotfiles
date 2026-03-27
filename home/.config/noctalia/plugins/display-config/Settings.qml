import QtQuick
import QtQuick.Layouts
import qs.Commons
import qs.Widgets

ColumnLayout {
  id: root

  property var pluginApi: null
  property var displayService: pluginApi?.mainInstance?.displayService || null

  property var cfg: pluginApi?.pluginSettings || ({})
  property var defaults: pluginApi?.manifest?.metadata?.defaultSettings || ({})

  spacing: Style.marginM

  NHeader {
    text: "Display Config Settings"
    Layout.fillWidth: true
  }

  NDivider {}

  NLabel {
    text: "Backend"
  }

  NComboBox {
    Layout.fillWidth: true
    model: ListModel {
      ListElement { key: "niri"; name: "niri" }
      ListElement { key: "hyprland"; name: "Hyprland (stub)" }
      ListElement { key: "sway"; name: "Sway (stub)" }
      ListElement { key: "wlr-randr"; name: "wlr-randr (stub)" }
    }
    currentKey: cfg.backend ?? defaults.backend
    onSelected: function (key) {
      cfg.backend = key;
      pluginApi?.saveSettings();
      displayService?.fetchOutputs();
    }
  }

  NLabel {
    text: "Poll interval (seconds)"
  }

  NSpinBox {
    from: 1
    to: 60
    Component.onCompleted: value = cfg.pollInterval ?? defaults.pollInterval
    onValueChanged: {
      cfg.pollInterval = value;
      pluginApi?.saveSettings();
    }
  }

  NLabel {
    text: "Icon color"
  }

  NColorChoice {
    selectedColor: cfg.iconColor ?? defaults.iconColor
    onColorSelected: function (color) {
      cfg.iconColor = color;
      pluginApi?.saveSettings();
    }
  }

  NDivider {}

  NLabel {
    text: "Presets (" + ((cfg.presets || []).length) + ")"
  }

  RowLayout {
    Layout.fillWidth: true
    spacing: Style.marginS

    NTextInput {
      id: presetNameInput
      Layout.fillWidth: true
      placeholderText: "Preset name"
    }

    NIconButton {
      icon: "device-floppy"
      tooltipText: "Save current layout as preset"
      enabled: presetNameInput.text.trim() !== ""
      onClicked: {
        displayService?.saveCurrentAsPreset(presetNameInput.text.trim());
        presetNameInput.text = "";
      }
    }
  }

  Repeater {
    model: cfg.presets || []

    delegate: RowLayout {
      Layout.fillWidth: true
      spacing: Style.marginS

      NText {
        text: modelData.name
        Layout.fillWidth: true
        color: Color.mOnSurface
      }

      NIconButton {
        icon: "player-play"
        tooltipText: "Apply preset"
        onClicked: displayService?.applyPreset(modelData)
      }

      NIconButton {
        icon: "trash"
        tooltipText: "Delete preset"
        onClicked: {
          var p = cfg.presets || [];
          p.splice(index, 1);
          cfg.presets = p;
          pluginApi?.saveSettings();
        }
      }
    }
  }
}
