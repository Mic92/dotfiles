import QtQuick
import QtQuick.Layouts
import org.kde.plasma.plasmoid
import org.kde.plasma.components as PlasmaComponents3

PlasmoidItem {
    id: root

    property bool hasError: false
    property bool isTracking: false
    property string currentActivity: ""
    property string currentProject: ""
    property string currentCustomer: ""
    property string currentDescription: ""
    property string startTime: ""
    property int elapsedSeconds: 0
    property string kimaiUrl: "https://kimai.thalheim.io"
    property string apiToken: ""

    // ID of the currently running timesheet (for stop).
    property int activeTimesheetId: -1
    // ID of the most recent timesheet (for restart when idle).
    property int lastTimesheetId: -1

    // Replaced at install time by install.sh with the actual token.
    readonly property string tokenPlaceholder: "@KIMAI_API_TOKEN@"

    Plasmoid.icon: hasError ? "dialog-error" : (isTracking ? "chronometer" : "chronometer-pause")

    toolTipMainText: {
        if (hasError) return "Kimai: connection error"
        if (!isTracking) return "Kimai: not tracking"
        return "Kimai: tracking"
    }

    toolTipSubText: {
        if (hasError) return "Could not connect to Kimai"
        if (!isTracking) return "Click to restart last timesheet"
        var lines = []
        if (currentCustomer) lines.push("Customer: " + currentCustomer)
        if (currentProject) lines.push("Project: " + currentProject)
        if (currentActivity) lines.push("Activity: " + currentActivity)
        if (currentDescription) lines.push("Description: " + currentDescription)
        lines.push("Elapsed: " + formatDuration(elapsedSeconds))
        lines.push("\nClick to stop")
        return lines.join("\n")
    }

    preferredRepresentation: compactRepresentation

    compactRepresentation: MouseArea {
        implicitWidth: row.implicitWidth
        implicitHeight: row.implicitHeight
        acceptedButtons: Qt.LeftButton | Qt.MiddleButton

        onClicked: function(mouse) {
            if (mouse.button === Qt.MiddleButton) {
                Qt.openUrlExternally(root.kimaiUrl)
                return
            }
            // Left click: toggle timer
            if (root.isTracking) {
                stopTimer()
            } else {
                startTimer()
            }
        }

        RowLayout {
            id: row
            anchors.centerIn: parent
            spacing: 4

            PlasmaComponents3.Label {
                text: {
                    if (root.hasError) return "!"
                    if (!root.isTracking) return "⏸"
                    return formatDuration(root.elapsedSeconds)
                }
                font.bold: root.isTracking
                color: root.hasError ? "red" : (root.isTracking ? "green" : "gray")
            }
        }
    }

    fullRepresentation: ColumnLayout {
        spacing: 8
        Layout.preferredWidth: 300

        PlasmaComponents3.Label {
            text: root.isTracking ? "Active Timesheet" : "No Active Timesheet"
            font.bold: true
        }

        PlasmaComponents3.Label {
            visible: root.isTracking
            text: {
                var lines = []
                if (root.currentCustomer) lines.push("Customer: " + root.currentCustomer)
                if (root.currentProject) lines.push("Project: " + root.currentProject)
                if (root.currentActivity) lines.push("Activity: " + root.currentActivity)
                if (root.currentDescription) lines.push("Description: " + root.currentDescription)
                lines.push("Elapsed: " + formatDuration(root.elapsedSeconds))
                return lines.join("\n")
            }
        }

        PlasmaComponents3.Button {
            text: root.isTracking ? "Stop" : "Start"
            icon.name: root.isTracking ? "media-playback-stop" : "media-playback-start"
            onClicked: {
                if (root.isTracking) {
                    stopTimer()
                } else {
                    startTimer()
                }
            }
        }
    }

    // Poll the Kimai API every 30s for active timesheets.
    Timer {
        interval: 30000
        repeat: true
        running: true
        triggeredOnStart: true
        onTriggered: fetchActiveTimesheet()
    }

    // Tick every second to keep the elapsed-time display live.
    Timer {
        interval: 1000
        repeat: true
        running: root.isTracking
        onTriggered: root.elapsedSeconds += 1
    }

    Component.onCompleted: {
        // The install script substitutes the placeholder with the real token.
        if (tokenPlaceholder !== "@" + "KIMAI_API_TOKEN" + "@") {
            root.apiToken = tokenPlaceholder
        }
    }

    function formatDuration(totalSeconds) {
        var h = Math.floor(totalSeconds / 3600)
        var m = Math.floor((totalSeconds % 3600) / 60)
        var pad = function(n) { return n < 10 ? "0" + n : "" + n }
        return h + ":" + pad(m)
    }

    function fetchActiveTimesheet() {
        if (!root.apiToken) {
            loadApiToken()
            if (!root.apiToken) {
                root.hasError = true
                return
            }
        }

        var xhr = new XMLHttpRequest()
        xhr.open("GET", root.kimaiUrl + "/api/timesheets/active", true)
        xhr.setRequestHeader("Authorization", "Bearer " + root.apiToken)

        xhr.onreadystatechange = function() {
            if (xhr.readyState !== XMLHttpRequest.DONE) return

            if (xhr.status !== 200) {
                root.hasError = true
                return
            }

            try {
                var timesheets = JSON.parse(xhr.responseText)
                root.hasError = false

                if (!Array.isArray(timesheets) || timesheets.length === 0) {
                    root.isTracking = false
                    root.activeTimesheetId = -1
                    root.currentActivity = ""
                    root.currentProject = ""
                    root.currentCustomer = ""
                    root.currentDescription = ""
                    root.startTime = ""
                    root.elapsedSeconds = 0
                    // Fetch the most recent timesheet so we can restart it.
                    fetchRecentTimesheet()
                    return
                }

                // Use the first active timesheet (most recent).
                var ts = timesheets[0]
                root.isTracking = true
                root.activeTimesheetId = ts.id
                root.lastTimesheetId = ts.id
                root.currentActivity = ts.activity ? (ts.activity.name || "") : ""
                root.currentProject = ts.project ? (ts.project.name || "") : ""
                root.currentCustomer = (ts.project && ts.project.customer) ? (ts.project.customer.name || "") : ""
                root.currentDescription = ts.description || ""
                root.startTime = ts.begin || ""

                // Compute elapsed seconds from the begin timestamp.
                if (root.startTime) {
                    var beginDate = new Date(root.startTime)
                    var now = new Date()
                    root.elapsedSeconds = Math.floor((now.getTime() - beginDate.getTime()) / 1000)
                }
            } catch (e) {
                root.hasError = true
            }
        }
        xhr.send()
    }

    function fetchRecentTimesheet() {
        var xhr = new XMLHttpRequest()
        xhr.open("GET", root.kimaiUrl + "/api/timesheets/recent?size=1", true)
        xhr.setRequestHeader("Authorization", "Bearer " + root.apiToken)

        xhr.onreadystatechange = function() {
            if (xhr.readyState !== XMLHttpRequest.DONE) return
            if (xhr.status !== 200) return

            try {
                var timesheets = JSON.parse(xhr.responseText)
                if (Array.isArray(timesheets) && timesheets.length > 0) {
                    root.lastTimesheetId = timesheets[0].id
                }
            } catch (e) {
                // ignore — we just won't be able to restart
            }
        }
        xhr.send()
    }

    function stopTimer() {
        if (root.activeTimesheetId < 0) return

        var xhr = new XMLHttpRequest()
        xhr.open("PATCH", root.kimaiUrl + "/api/timesheets/" + root.activeTimesheetId + "/stop", true)
        xhr.setRequestHeader("Authorization", "Bearer " + root.apiToken)
        xhr.setRequestHeader("Content-Type", "application/json")

        xhr.onreadystatechange = function() {
            if (xhr.readyState !== XMLHttpRequest.DONE) return
            // Refresh state regardless of outcome.
            fetchActiveTimesheet()
        }
        xhr.send()
    }

    function startTimer() {
        if (root.lastTimesheetId < 0) return

        // Restart the most recent timesheet — the API creates a new
        // timesheet entry with the same project/activity.
        var xhr = new XMLHttpRequest()
        xhr.open("PATCH", root.kimaiUrl + "/api/timesheets/" + root.lastTimesheetId + "/restart", true)
        xhr.setRequestHeader("Authorization", "Bearer " + root.apiToken)
        xhr.setRequestHeader("Content-Type", "application/json")

        xhr.onreadystatechange = function() {
            if (xhr.readyState !== XMLHttpRequest.DONE) return
            // Refresh state regardless of outcome.
            fetchActiveTimesheet()
        }
        xhr.send()
    }
}
