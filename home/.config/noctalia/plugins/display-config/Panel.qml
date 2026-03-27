import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.Commons
import qs.Widgets

Item {
  id: root

  property var pluginApi: null
  property var displayService: pluginApi?.mainInstance?.displayService || null

  readonly property var outputs: displayService?.outputs ?? []
  readonly property int outputCount: displayService?.outputCount ?? 0
  readonly property int enabledCount: displayService?.enabledCount ?? 0
  readonly property string fetchState: displayService?.fetchState ?? "idle"

  property var cfg: pluginApi?.pluginSettings || ({})

  implicitWidth: 560
  implicitHeight: contentColumn.implicitHeight + Style.marginL * 2

  // Revert-countdown state mirrored from the service so the confirm bar
  // lives inside the panel instead of only in a toast (which might land on
  // a monitor that just went dark).
  readonly property bool revertPending: displayService?.revertPending ?? false
  readonly property int revertSeconds: displayService?.revertSecondsLeft ?? 0

  ColumnLayout {
    id: contentColumn
    anchors.fill: parent
    anchors.margins: Style.marginL
    spacing: Style.marginM

    // Header
    RowLayout {
      Layout.fillWidth: true
      spacing: Style.marginM

      NIcon {
        icon: "device-desktop"
        pointSize: Style.fontSizeXL
        color: Color.mPrimary
      }

      NText {
        text: {
          if (root.fetchState === "error")
            return "Error querying outputs";
          return root.enabledCount + "/" + root.outputCount + " output" + (root.outputCount !== 1 ? "s" : "") + " enabled";
        }
        font.pixelSize: Style.fontSizeL
        font.bold: true
        color: Color.mOnSurface
        Layout.fillWidth: true
      }

      NIconButton {
        icon: "refresh"
        baseSize: 32
        tooltipText: "Refresh"
        onClicked: displayService?.fetchOutputs()
      }

      NIconButton {
        icon: "external-link"
        baseSize: 32
        tooltipText: "Open wdisplays"
        onClicked: {
          wdisplaysLauncher.startDetached();
          pluginApi?.closePanel(pluginApi.panelOpenScreen);
        }
      }
    }

    // Launch wdisplays for drag-and-drop arrangement — the proper tool for
    // 3+ monitor layouts that the quick-arrange buttons can't handle.
    // startDetached() is required: running=true ties the child to this
    // Process object, which is destroyed the moment closePanel() tears down
    // the panel delegate, killing wdisplays before its window appears.
    Process {
      id: wdisplaysLauncher
      command: ["sh", "-c", "command -v wdisplays >/dev/null && exec wdisplays || notify-send 'wdisplays not installed'"]
    }

    // Sticky confirm bar — shows whenever a change is pending revert.
    Rectangle {
      Layout.fillWidth: true
      Layout.preferredHeight: confirmRow.implicitHeight + Style.marginM * 2
      visible: root.revertPending
      radius: Style.radiusM
      color: Color.mTertiary

      RowLayout {
        id: confirmRow
        anchors.fill: parent
        anchors.margins: Style.marginM
        spacing: Style.marginM

        NIcon {
          icon: "alert-triangle"
          pointSize: Style.fontSizeL
          color: Color.mOnTertiary
        }

        NText {
          text: "Reverting in " + root.revertSeconds + "s"
          font.pixelSize: Style.fontSizeM
          font.bold: true
          color: Color.mOnTertiary
          Layout.fillWidth: true
        }

        NButton {
          text: "Revert"
          icon: "restore"
          outlined: true
          onClicked: displayService?.doRevert()
        }

        NButton {
          text: "Keep"
          icon: "check"
          onClicked: displayService?.confirmRevert()
        }
      }
    }

    NDivider {}

    // Output list
    Flickable {
      Layout.fillWidth: true
      Layout.fillHeight: true
      Layout.preferredHeight: Math.min(outputList.implicitHeight, 500)
      contentHeight: outputList.implicitHeight
      clip: true

      ColumnLayout {
        id: outputList
        width: parent.width
        spacing: Style.marginM

        // Error state
        NText {
          visible: root.fetchState === "error"
          text: displayService?.errorMessage ?? "Unknown error"
          color: Color.mError
          Layout.fillWidth: true
          wrapMode: Text.WordWrap
        }

        // Per-output cards
        Repeater {
          model: root.outputs

          delegate: Rectangle {
            id: outputCard

            Layout.fillWidth: true
            Layout.preferredHeight: cardColumn.implicitHeight + Style.marginM * 2
            radius: Style.radiusM
            color: Color.mSurfaceVariant

            ColumnLayout {
              id: cardColumn
              anchors.fill: parent
              anchors.margins: Style.marginM
              spacing: Style.marginS

              // Title row: name + make/model + power toggle
              RowLayout {
                Layout.fillWidth: true
                spacing: Style.marginS

                NIcon {
                  icon: modelData.enabled ? "device-desktop" : "device-desktop-off"
                  pointSize: Style.fontSizeL
                  color: modelData.enabled ? Color.mPrimary : Color.mOnSurfaceVariant
                }

                ColumnLayout {
                  Layout.fillWidth: true
                  spacing: 0

                  NText {
                    text: modelData.name
                    font.bold: true
                    font.pixelSize: Style.fontSizeM
                    color: Color.mOnSurface
                  }

                  NText {
                    visible: text !== ""
                    text: [modelData.make, modelData.model].filter(function (s) {
                      return s && s.trim() !== "";
                    }).join(" ")
                    font.pixelSize: Style.fontSizeXS
                    color: Color.mOnSurfaceVariant
                  }
                }

                NToggle {
                  checked: modelData.enabled
                  onToggled: function (checked) {
                    displayService?.applyOutput(modelData.name, {
                                                  "enabled": checked
                                                }, true);
                  }
                }
              }

              // Mode selector — applies immediately; a revert-countdown toast
              // rolls the change back unless confirmed.
              RowLayout {
                Layout.fillWidth: true
                visible: modelData.enabled
                spacing: Style.marginS

                NText {
                  text: "Mode"
                  font.pixelSize: Style.fontSizeS
                  color: Color.mOnSurfaceVariant
                  Layout.preferredWidth: 60
                }

                NComboBox {
                  Layout.fillWidth: true
                  // Group modes by resolution so the dropdown isn't a wall of
                  // near-duplicate 59.94/59.95/60.00 Hz entries. The first
                  // (highest-refresh) variant per resolution shows as
                  // "3840×1600", later ones as indented "  @59.94".
                  model: {
                    var lm = [];
                    var seenRes = {};
                    for (var i = 0; i < modelData.modes.length; i++) {
                      var m = modelData.modes[i];
                      var res = m.width + "×" + m.height;
                      var hz = m.refresh.toFixed(2).replace(/\.?0+$/, "");
                      var label;
                      if (!seenRes[res]) {
                        seenRes[res] = true;
                        label = res + "  " + hz + "Hz" + (m.preferred ? " ★" : "");
                      } else {
                        label = "    " + hz + "Hz" + (m.preferred ? " ★" : "");
                      }
                      lm.push({
                                "key": m.key,
                                "name": label
                              });
                    }
                    return lm;
                  }
                  currentKey: modelData.currentMode || ""
                  onSelected: function (key) {
                    if (key !== modelData.currentMode) {
                      displayService?.applyOutput(modelData.name, {
                                                    "mode": key
                                                  }, true);
                    }
                  }
                }
              }

              // Scale — applies on edit, debounced so clicking the spin arrows
              // several times results in one niri call instead of a cascade.
              RowLayout {
                Layout.fillWidth: true
                visible: modelData.enabled
                spacing: Style.marginS

                NText {
                  text: "Scale"
                  font.pixelSize: Style.fontSizeS
                  color: Color.mOnSurfaceVariant
                  Layout.preferredWidth: 60
                }

                NSpinBox {
                  id: scaleSpin
                  from: 50
                  to: 300
                  stepSize: 25
                  suffix: "%"
                  property bool ready: false
                  Component.onCompleted: {
                    value = Math.round(modelData.scale * 100);
                    ready = true;
                  }
                  onValueChanged: {
                    if (ready) {
                      scaleDebounce.restart();
                    }
                  }
                }

                Timer {
                  id: scaleDebounce
                  interval: 400
                  onTriggered: {
                    var newScale = scaleSpin.value / 100.0;
                    if (Math.abs(newScale - modelData.scale) > 0.001) {
                      displayService?.applyOutput(modelData.name, {
                                                    "scale": newScale
                                                  }, true);
                    }
                  }
                }

                Item {
                  Layout.fillWidth: true
                }
              }
            }
          }
        }

        // With 3+ outputs the pairwise arrange math would leave the extras
        // overlapping — point at the tools that actually handle N monitors.
        ColumnLayout {
          Layout.fillWidth: true
          visible: root.outputCount > 2
          spacing: Style.marginS

          NDivider {}

          NText {
            text: root.outputCount + " monitors — use wdisplays to arrange, then save as a preset"
            font.pixelSize: Style.fontSizeS
            color: Color.mOnSurfaceVariant
            Layout.fillWidth: true
            wrapMode: Text.WordWrap
          }

          NButton {
            Layout.fillWidth: true
            icon: "external-link"
            text: "Open wdisplays"
            onClicked: {
              wdisplaysLauncher.startDetached();
              pluginApi?.closePanel(pluginApi.panelOpenScreen);
            }
          }
        }

        // Arrange — one-click layouts computed from logical sizes so nobody
        // has to reason about x/y coordinates. Gated to exactly two outputs
        // like KDE's Super+P OSD: the preset math only positions a pair, and
        // silently leaving a third monitor overlapping is worse than hiding
        // the buttons. For 3+ monitors, use saved presets instead.
        ColumnLayout {
          Layout.fillWidth: true
          visible: root.outputCount === 2
          spacing: Style.marginS

          NDivider {}

          NText {
            text: "Arrange"
            font.bold: true
            font.pixelSize: Style.fontSizeM
            color: Color.mOnSurface
          }

          GridLayout {
            Layout.fillWidth: true
            columns: 2
            rowSpacing: Style.marginS
            columnSpacing: Style.marginS

            NButton {
              Layout.fillWidth: true
              icon: "arrow-bar-right"
              text: "Extend right"
              onClicked: displayService?.applyArrangement("extend-right")
            }
            NButton {
              Layout.fillWidth: true
              icon: "arrow-bar-left"
              text: "Extend left"
              onClicked: displayService?.applyArrangement("extend-left")
            }
            NButton {
              Layout.fillWidth: true
              icon: "arrow-bar-to-up"
              text: "External above"
              onClicked: displayService?.applyArrangement("stack-above")
            }
            NButton {
              Layout.fillWidth: true
              icon: "arrow-bar-to-down"
              text: "External below"
              onClicked: displayService?.applyArrangement("stack-below")
            }
            NButton {
              Layout.fillWidth: true
              icon: "device-desktop"
              text: "External only"
              onClicked: displayService?.applyArrangement("external-only")
            }
            NButton {
              Layout.fillWidth: true
              icon: "device-laptop"
              text: "Laptop only"
              onClicked: displayService?.applyArrangement("internal-only")
            }
          }
        }

        // Presets
        ColumnLayout {
          Layout.fillWidth: true
          visible: (cfg.presets || []).length > 0
          spacing: Style.marginS

          NDivider {}

          NText {
            text: "Presets"
            font.bold: true
            font.pixelSize: Style.fontSizeM
            color: Color.mOnSurface
          }

          Flow {
            Layout.fillWidth: true
            spacing: Style.marginS

            Repeater {
              model: cfg.presets || []

              delegate: NButton {
                icon: "layout"
                text: modelData.name
                onClicked: displayService?.applyPreset(modelData)
              }
            }
          }
        }
      }
    }
  }
}
