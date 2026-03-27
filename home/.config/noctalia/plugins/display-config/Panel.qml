import QtQuick
import QtQuick.Layouts
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

  implicitWidth: 460
  implicitHeight: contentColumn.implicitHeight + Style.marginL * 2

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

            // Pending edits (applied on "Apply" button)
            property string pendingMode: modelData.currentMode || ""
            property real pendingScale: modelData.scale
            property int pendingX: modelData.position.x
            property int pendingY: modelData.position.y
            property string pendingTransform: modelData.transform
            property bool showAdvanced: false

            readonly property bool dirty: pendingMode !== (modelData.currentMode || "")
                                          || Math.abs(pendingScale - modelData.scale) > 0.001
                                          || pendingX !== modelData.position.x
                                          || pendingY !== modelData.position.y
                                          || pendingTransform !== modelData.transform

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
                                                });
                  }
                }
              }

              // Mode selector
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
                  model: {
                    var lm = [];
                    for (var i = 0; i < modelData.modes.length; i++) {
                      var m = modelData.modes[i];
                      lm.push({
                                "key": m.key,
                                "name": m.key + (m.preferred ? " ★" : "")
                              });
                    }
                    return lm;
                  }
                  currentKey: outputCard.pendingMode
                  onSelected: function (key) {
                    outputCard.pendingMode = key;
                  }
                }
              }

              // Scale
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
                  from: 50
                  to: 300
                  stepSize: 25
                  value: Math.round(outputCard.pendingScale * 100)
                  onValueChanged: outputCard.pendingScale = value / 100.0
                }

                NText {
                  text: outputCard.pendingScale.toFixed(2) + "×"
                  font.pixelSize: Style.fontSizeS
                  color: Color.mOnSurfaceVariant
                }

                Item {
                  Layout.fillWidth: true
                }

                NIconButton {
                  icon: outputCard.showAdvanced ? "chevron-up" : "chevron-down"
                  baseSize: 24
                  tooltipText: "Position & transform"
                  onClicked: outputCard.showAdvanced = !outputCard.showAdvanced
                }
              }

              // Advanced: position + transform
              ColumnLayout {
                Layout.fillWidth: true
                visible: modelData.enabled && outputCard.showAdvanced
                spacing: Style.marginS

                RowLayout {
                  Layout.fillWidth: true
                  spacing: Style.marginS

                  NText {
                    text: "Position"
                    font.pixelSize: Style.fontSizeS
                    color: Color.mOnSurfaceVariant
                    Layout.preferredWidth: 60
                  }

                  NSpinBox {
                    from: -32768
                    to: 32768
                    stepSize: 10
                    value: outputCard.pendingX
                    onValueChanged: outputCard.pendingX = value
                  }

                  NSpinBox {
                    from: -32768
                    to: 32768
                    stepSize: 10
                    value: outputCard.pendingY
                    onValueChanged: outputCard.pendingY = value
                  }
                }

                RowLayout {
                  Layout.fillWidth: true
                  spacing: Style.marginS

                  NText {
                    text: "Rotate"
                    font.pixelSize: Style.fontSizeS
                    color: Color.mOnSurfaceVariant
                    Layout.preferredWidth: 60
                  }

                  NComboBox {
                    Layout.fillWidth: true
                    model: [{
                        "key": "normal",
                        "name": "Normal"
                      }, {
                        "key": "90",
                        "name": "90°"
                      }, {
                        "key": "180",
                        "name": "180°"
                      }, {
                        "key": "270",
                        "name": "270°"
                      }, {
                        "key": "flipped",
                        "name": "Flipped"
                      }, {
                        "key": "flipped-90",
                        "name": "Flipped 90°"
                      }, {
                        "key": "flipped-180",
                        "name": "Flipped 180°"
                      }, {
                        "key": "flipped-270",
                        "name": "Flipped 270°"
                      }]
                    currentKey: outputCard.pendingTransform
                    onSelected: function (key) {
                      outputCard.pendingTransform = key;
                    }
                  }
                }
              }

              // Apply / Reset
              RowLayout {
                Layout.fillWidth: true
                visible: modelData.enabled && outputCard.dirty
                spacing: Style.marginS

                Item {
                  Layout.fillWidth: true
                }

                NIconButton {
                  icon: "restore"
                  baseSize: 28
                  tooltipText: "Reset"
                  onClicked: {
                    outputCard.pendingMode = modelData.currentMode || "";
                    outputCard.pendingScale = modelData.scale;
                    outputCard.pendingX = modelData.position.x;
                    outputCard.pendingY = modelData.position.y;
                    outputCard.pendingTransform = modelData.transform;
                  }
                }

                NIconButton {
                  icon: "check"
                  baseSize: 28
                  tooltipText: "Apply"
                  onClicked: {
                    displayService?.applyOutput(modelData.name, {
                                                  "mode": outputCard.pendingMode,
                                                  "scale": outputCard.pendingScale,
                                                  "x": outputCard.pendingX,
                                                  "y": outputCard.pendingY,
                                                  "transform": outputCard.pendingTransform
                                                });
                  }
                }
              }
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
