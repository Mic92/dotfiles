import QtQuick
import QtQuick.Layouts
import qs.Commons
import qs.Widgets

Item {
  id: root

  property var pluginApi: null
  property var alertService: pluginApi?.mainInstance?.alertService || null

  readonly property var activeAlerts: alertService?.activeAlerts ?? []
  readonly property int alertCount: alertService?.alertCount ?? 0
  readonly property string fetchState: alertService?.fetchState ?? "idle"

  // Group alerts by alertname for sectioned display
  readonly property var groupedAlerts: {
    var alerts = root.activeAlerts;
    var groups = {};
    var order = [];
    for (var i = 0; i < alerts.length; i++) {
      var name = alerts[i].labels.alertname || "Unknown";
      if (!groups[name]) {
        groups[name] = [];
        order.push(name);
      }
      groups[name].push(alerts[i]);
    }
    var result = [];
    for (var j = 0; j < order.length; j++) {
      result.push({ name: order[j], alerts: groups[order[j]] });
    }
    return result;
  }

  implicitWidth: 420
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
        icon: root.alertCount > 0 ? "alert-circle" : "circle-check"
        pointSize: Style.fontSizeXL
        color: root.alertCount > 0 ? Color.mError : Color.mPrimary
      }

      NText {
        text: {
          if (root.fetchState === "error")
            return "Error fetching alerts";
          if (root.alertCount === 0)
            return "All clear — no active alerts";
          return root.alertCount + " active alert" + (root.alertCount !== 1 ? "s" : "");
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
        onClicked: alertService?.fetchAlerts()
      }

      NIconButton {
        icon: "external-link"
        baseSize: 32
        tooltipText: "Open Alertmanager"
        onClicked: {
          var cfg = pluginApi?.pluginSettings || {};
          var defaults = pluginApi?.manifest?.metadata?.defaultSettings || {};
          var url = cfg.alertmanagerUrl ?? defaults.alertmanagerUrl;
          Qt.openUrlExternally(url);
        }
      }
    }

    NDivider {}

    // Alert list
    Flickable {
      Layout.fillWidth: true
      Layout.fillHeight: true
      Layout.preferredHeight: Math.min(alertList.implicitHeight, 400)
      contentHeight: alertList.implicitHeight
      clip: true

      ColumnLayout {
        id: alertList
        width: parent.width
        spacing: Style.marginS

        // Error state
        NText {
          visible: root.fetchState === "error"
          text: alertService?.errorMessage ?? "Unknown error"
          color: Color.mError
          Layout.fillWidth: true
          wrapMode: Text.WordWrap
        }

        // Empty state
        NText {
          visible: root.fetchState === "success" && root.alertCount === 0
          text: "✅ No active alerts"
          color: Color.mOnSurfaceVariant
          Layout.fillWidth: true
          horizontalAlignment: Text.AlignHCenter
          font.pixelSize: Style.fontSizeL
        }

        // Grouped alert sections
        Repeater {
          model: root.groupedAlerts

          delegate: ColumnLayout {
            id: groupDelegate

            property bool expanded: false

            Layout.fillWidth: true
            spacing: Style.marginXS

            // Section header (clickable to fold/unfold)
            Rectangle {
              Layout.fillWidth: true
              Layout.preferredHeight: sectionHeaderRow.implicitHeight + Style.marginS * 2
              Layout.topMargin: index > 0 ? Style.marginS : 0
              radius: Style.radiusS
              color: sectionHeaderMouse.containsMouse ? Color.mSurfaceVariant : "transparent"

              RowLayout {
                id: sectionHeaderRow
                anchors.fill: parent
                anchors.leftMargin: Style.marginS
                anchors.rightMargin: Style.marginS
                anchors.topMargin: Style.marginS
                anchors.bottomMargin: Style.marginS
                spacing: Style.marginS

                NIcon {
                  icon: groupDelegate.expanded ? "chevron-down" : "chevron-right"
                  pointSize: Style.fontSizeS
                  color: Color.mOnSurfaceVariant
                }

                NText {
                  text: modelData.name + " (" + modelData.alerts.length + ")"
                  font.bold: true
                  font.pixelSize: Style.fontSizeM
                  color: Color.mOnSurface
                  Layout.fillWidth: true
                }
              }

              MouseArea {
                id: sectionHeaderMouse
                anchors.fill: parent
                cursorShape: Qt.PointingHandCursor
                hoverEnabled: true
                onClicked: groupDelegate.expanded = !groupDelegate.expanded
              }
            }

            // Alerts in this section (only visible when expanded)
            Repeater {
              model: groupDelegate.expanded ? modelData.alerts : []

              delegate: Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: alertItemColumn.implicitHeight + Style.marginM * 2
                radius: Style.radiusM
                color: Color.mSurfaceVariant

                ColumnLayout {
                  id: alertItemColumn
                  anchors.fill: parent
                  anchors.margins: Style.marginM
                  spacing: Style.marginXS

                  RowLayout {
                    Layout.fillWidth: true
                    spacing: Style.marginS

                    NIcon {
                      icon: {
                        var severity = modelData.labels.severity || "warning";
                        if (severity === "critical") return "alert-octagon";
                        if (severity === "warning") return "alert-triangle";
                        return "info";
                      }
                      pointSize: Style.fontSizeM
                      color: {
                        var severity = modelData.labels.severity || "warning";
                        if (severity === "critical") return Color.mError;
                        if (severity === "warning") return Color.mTertiary;
                        return Color.mPrimary;
                      }
                    }

                    NText {
                      text: modelData.labels.host || modelData.labels.instance || ""
                      font.pixelSize: Style.fontSizeS
                      color: Color.mOnSurface
                      Layout.fillWidth: true
                    }
                  }

                  NText {
                    visible: text !== ""
                    text: {
                      if (modelData.annotations) {
                        return modelData.annotations.description
                          || modelData.annotations.summary
                          || "";
                      }
                      return "";
                    }
                    font.pixelSize: Style.fontSizeS
                    color: Color.mOnSurfaceVariant
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    maximumLineCount: 3
                    elide: Text.ElideRight
                  }
                }

                MouseArea {
                  anchors.fill: parent
                  cursorShape: Qt.PointingHandCursor
                  onClicked: {
                    if (modelData.generatorURL) {
                      Qt.openUrlExternally(modelData.generatorURL);
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
