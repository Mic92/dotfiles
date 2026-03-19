import QtQuick
import Quickshell.Io
import qs.Commons

Item {
  id: root

  property var pluginApi: null

  // Expose service to bar widget via mainInstance
  property alias alertService: alertService

  QtObject {
    id: alertService

    property var activeAlerts: []
    property int alertCount: 0
    property string fetchState: "idle" // "idle", "loading", "success", "error"
    property string errorMessage: ""

    function fetchAlerts() {
      var cfg = pluginApi?.pluginSettings || {};
      var defaults = pluginApi?.manifest?.metadata?.defaultSettings || {};
      var url = cfg.alertmanagerUrl ?? defaults.alertmanagerUrl;

      fetchState = "loading";

      var fetchCmd = "wget -q -O - '" + url + "/api/v2/alerts' 2>/dev/null";

      fetchProcess.command = ["sh", "-c", fetchCmd];
      fetchProcess.running = true;
    }
  }

  Process {
    id: fetchProcess
    stdout: StdioCollector {}

    onExited: function(exitCode) {
      if (exitCode !== 0) {
        alertService.fetchState = "error";
        alertService.errorMessage = "Failed to fetch alerts (exit " + exitCode + ")";
        Logger.w("Alertmanager", "Fetch failed with exit code:", exitCode);
        return;
      }

      var response = stdout.text;
      if (!response || response.trim() === "") {
        alertService.fetchState = "error";
        alertService.errorMessage = "Empty response";
        return;
      }

      try {
        var alerts = JSON.parse(response);
        if (Array.isArray(alerts)) {
          // Filter: active, not silenced, not inhibited, not muted
          var active = alerts.filter(function(alert) {
            return alert.status.state === "active"
                && alert.status.silencedBy.length === 0
                && alert.status.inhibitedBy.length === 0
                && (!alert.status.mutedBy || alert.status.mutedBy.length === 0);
          });
          active.sort(function(a, b) {
            var nameA = (a.labels.alertname || "").toLowerCase();
            var nameB = (b.labels.alertname || "").toLowerCase();
            if (nameA !== nameB) return nameA < nameB ? -1 : 1;
            var hostA = (a.labels.host || "").toLowerCase();
            var hostB = (b.labels.host || "").toLowerCase();
            if (hostA !== hostB) return hostA < hostB ? -1 : 1;
            return 0;
          });
          alertService.activeAlerts = active;
          alertService.alertCount = active.length;
          alertService.fetchState = "success";
          Logger.d("Alertmanager", "Fetched", active.length, "active alerts");
        }
      } catch (e) {
        alertService.fetchState = "error";
        alertService.errorMessage = "Parse error: " + e;
        Logger.e("Alertmanager", "Failed to parse alerts:", e);
      }
    }
  }

  Timer {
    id: pollTimer
    repeat: true
    running: pluginApi !== null
    triggeredOnStart: true
    interval: {
      var cfg = pluginApi?.pluginSettings || {};
      var defaults = pluginApi?.manifest?.metadata?.defaultSettings || {};
      var secs = cfg.pollInterval ?? defaults.pollInterval;
      return secs * 1000;
    }
    onTriggered: alertService.fetchAlerts()
  }

  IpcHandler {
    target: "plugin:alertmanager"
    function refresh() {
      alertService.fetchAlerts();
    }
    function toggle() {
      if (pluginApi) {
        pluginApi.withCurrentScreen(function(screen) {
          pluginApi.togglePanel(screen);
        });
      }
    }
  }
}
