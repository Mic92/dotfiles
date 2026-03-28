import QtQuick
import Quickshell.Io

Item {
    id: root
    property var pluginApi: null

    IpcHandler {
        target: "plugin:internet-provider"

        function toggle() {
            if (!pluginApi) return;
            pluginApi.withCurrentScreen(screen => {
                pluginApi.toggleLauncher(screen);
            });
        }
    }
}
