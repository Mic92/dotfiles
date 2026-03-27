import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Modules.Bar.Extras
import qs.Services.UI
import qs.Widgets

Item {
  id: root

  property var pluginApi: null
  property var displayService: pluginApi?.mainInstance?.displayService || null

  property ShellScreen screen
  property string widgetId: ""
  property string section: ""

  property var cfg: pluginApi?.pluginSettings || ({})
  property var defaults: pluginApi?.manifest?.metadata?.defaultSettings || ({})

  readonly property string iconColorKey: cfg.iconColor ?? defaults.iconColor
  readonly property int enabledCount: displayService?.enabledCount ?? 0
  readonly property int outputCount: displayService?.outputCount ?? 0
  readonly property string fetchState: displayService?.fetchState ?? "idle"
  readonly property bool countChanged: displayService?.countChanged ?? false

  readonly property string currentIcon: {
    if (fetchState === "loading")
      return "loader";
    if (fetchState === "error")
      return "alert-triangle";
    if (enabledCount < outputCount)
      return "device-desktop-off";
    return "device-desktop";
  }

  readonly property color iconColor: {
    if (fetchState === "error")
      return Color.mError;
    if (countChanged)
      return Color.mTertiary;
    return Color.resolveColorKey(iconColorKey);
  }

  implicitWidth: pill.width
  implicitHeight: pill.height

  BarPill {
    id: pill
    screen: root.screen
    oppositeDirection: BarService.getPillDirection(root)
    icon: root.currentIcon
    text: root.enabledCount.toString()
    forceOpen: root.countChanged
    autoHide: true
    customTextIconColor: root.iconColor

    onClicked: {
      if (pluginApi) {
        pluginApi.openPanel(root.screen, root);
      }
    }

    onRightClicked: {
      PanelService.showContextMenu(contextMenu, root, screen);
    }
  }

  Process {
    id: wdisplaysLauncher
    command: ["sh", "-c", "command -v wdisplays >/dev/null && exec wdisplays || notify-send 'wdisplays not installed'"]
  }

  NPopupContextMenu {
    id: contextMenu

    model: {
      var m = [];
      if (root.outputCount === 2) {
        m.push({
                 "label": "Extend right",
                 "action": "arrange:extend-right",
                 "icon": "arrow-bar-right"
               });
        m.push({
                 "label": "Extend left",
                 "action": "arrange:extend-left",
                 "icon": "arrow-bar-left"
               });
        m.push({
                 "label": "External only",
                 "action": "arrange:external-only",
                 "icon": "device-desktop"
               });
        m.push({
                 "label": "Laptop only",
                 "action": "arrange:internal-only",
                 "icon": "device-laptop"
               });
      }
      var presets = cfg.presets || [];
      for (var i = 0; i < presets.length; i++) {
        m.push({
                 "label": "Preset: " + presets[i].name,
                 "action": "preset:" + presets[i].name,
                 "icon": "layout"
               });
      }
      m.push({
               "label": "Open wdisplays",
               "action": "wdisplays",
               "icon": "external-link"
             });
      m.push({
               "label": "Refresh",
               "action": "refresh",
               "icon": "refresh"
             });
      m.push({
               "label": "Settings",
               "action": "settings",
               "icon": "settings"
             });
      return m;
    }

    onTriggered: function (action) {
      contextMenu.close();
      PanelService.closeContextMenu(screen);
      if (action === "refresh") {
        displayService?.fetchOutputs();
      } else if (action === "wdisplays") {
        wdisplaysLauncher.startDetached();
      } else if (action.indexOf("arrange:") === 0) {
        displayService?.applyArrangement(action.substring(8));
      } else if (action.indexOf("preset:") === 0) {
        var name = action.substring(7);
        var presets = cfg.presets || [];
        for (var i = 0; i < presets.length; i++) {
          if (presets[i].name === name) {
            displayService?.applyPreset(presets[i]);
            break;
          }
        }
      } else if (action === "settings") {
        BarService.openPluginSettings(root.screen, pluginApi.manifest);
      }
    }
  }
}
