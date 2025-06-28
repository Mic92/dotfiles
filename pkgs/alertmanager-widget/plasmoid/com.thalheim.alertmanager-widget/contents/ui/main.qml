import QtQuick
import QtQuick.Layouts
import org.kde.plasma.plasmoid
import org.kde.plasma.components as PlasmaComponents3
import Qt.labs.platform as Platform

PlasmoidItem {
    id: root

    property int alertCount: 0
    property bool hasError: false
    property string alertmanagerUrl: "http://alertmanager.r"
    property var activeAlerts: []

    Plasmoid.icon: hasError ? "dialog-error" : (alertCount > 0 ? "dialog-warning" : "security-high")

    toolTipMainText: hasError ? "Error fetching alerts" : ("Active Alerts: " + alertCount)
    toolTipSubText: {
        if (hasError) {
            return "Could not connect to Alertmanager"
        } else if (activeAlerts.length === 0) {
            return "No active alerts"
        } else {
            var text = ""
            for (var i = 0; i < activeAlerts.length && i < 10; i++) {
                var alert = activeAlerts[i]
                text += "• " + (alert.labels.alertname || "Unknown") + ": " + (alert.labels.instance || "N/A")
                if (alert.labels.severity) {
                    text += " [" + alert.labels.severity + "]"
                }
                if (i < activeAlerts.length - 1 && i < 9) {
                    text += "\n"
                }
            }
            if (activeAlerts.length > 10) {
                text += "\n... and " + (activeAlerts.length - 10) + " more"
            }
            return text
        }
    }

    preferredRepresentation: compactRepresentation

    compactRepresentation: MouseArea {
        implicitWidth: row.implicitWidth
        implicitHeight: row.implicitHeight

        onClicked: {
            Qt.openUrlExternally(root.alertmanagerUrl)
        }

        RowLayout {
            id: row
            anchors.centerIn: parent
            spacing: 2

            PlasmaComponents3.Label {
                text: root.hasError ? "!" : root.alertCount.toString()
                font.bold: root.alertCount > 0
                color: root.hasError ? "red" : (root.alertCount > 0 ? "orange" : "green")
            }
        }
    }

    fullRepresentation: PlasmaComponents3.Label {
        text: "Alerts: " + root.alertCount
    }

    Timer {
        interval: 30000
        repeat: true
        running: true
        triggeredOnStart: true
        onTriggered: fetchAlerts()
    }

    function fetchAlerts() {
        var xhr = new XMLHttpRequest();
        var url = root.alertmanagerUrl + "/api/v2/alerts";

        xhr.open("GET", url, true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState === XMLHttpRequest.DONE) {
                if (xhr.status === 200) {
                    try {
                        var alerts = JSON.parse(xhr.responseText);
                        if (Array.isArray(alerts)) {
                            // Filter alerts that are not silenced and not resolved
                            root.activeAlerts = alerts.filter(function(alert) {
                                return alert.status.state === "active" &&
                                       alert.status.silencedBy.length === 0;
                            });
                            root.alertCount = root.activeAlerts.length;
                            root.hasError = false;
                        }
                    } catch (e) {
                        root.hasError = true;
                    }
                } else {
                    root.hasError = true;
                }
            }
        };
        xhr.send();
    }
}
