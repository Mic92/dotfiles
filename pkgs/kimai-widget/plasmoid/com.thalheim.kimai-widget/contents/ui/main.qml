import QtQuick
import QtQuick.Layouts
import QtQuick.Controls as QQC2
import org.kde.plasma.plasmoid
import org.kde.plasma.components as PlasmaComponents3
import org.kde.kirigami as Kirigami

PlasmoidItem {
    id: root

    property bool hasError: false
    property bool isTracking: false
    property int elapsedSeconds: 0
    property string kimaiUrl: "https://kimai.thalheim.io"
    property string apiToken: ""

    // Today's timesheet entries (full objects from the API).
    property var todayEntries: []
    // Index of the currently running entry in todayEntries (-1 if none).
    property int activeIndex: -1

    // Combined project+activity choices for new entry creation.
    // Each entry has: projectId, activityId, label
    ListModel { id: taskModel }

    // Last-used project/activity IDs (from the most recent timesheet).
    property int lastProjectId: -1
    property int lastActivityId: -1

    // Replaced at install time by install.sh with the actual token.
    readonly property string tokenPlaceholder: "@KIMAI_API_TOKEN@"

    Plasmoid.icon: hasError ? "dialog-error" : (isTracking ? "chronometer" : "chronometer-pause")

    toolTipMainText: {
        if (hasError) return "Kimai: connection error"
        if (!isTracking) return "Kimai: not tracking"
        return "Kimai: tracking " + formatDuration(elapsedSeconds)
    }
    toolTipSubText: {
        if (hasError) return "Could not connect to Kimai"
        return "Today: " + formatDuration(todayTotal()) + " total"
    }

    preferredRepresentation: compactRepresentation

    // ── Compact (panel) ────────────────────────────────────────────
    compactRepresentation: MouseArea {
        implicitWidth: row.implicitWidth
        implicitHeight: row.implicitHeight

        onClicked: root.expanded = !root.expanded

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

    // ── Full (popup) ───────────────────────────────────────────────
    fullRepresentation: ColumnLayout {
        Layout.preferredWidth: Kirigami.Units.gridUnit * 22
        Layout.preferredHeight: Kirigami.Units.gridUnit * 20
        Layout.minimumWidth: Kirigami.Units.gridUnit * 18
        spacing: 0

        // Header
        RowLayout {
            Layout.fillWidth: true
            Layout.margins: Kirigami.Units.smallSpacing

            PlasmaComponents3.Label {
                text: "Today"
                font.bold: true
                Layout.fillWidth: true
            }

            PlasmaComponents3.Label {
                text: formatDuration(todayTotal())
                font.bold: true
            }

            PlasmaComponents3.ToolButton {
                icon.name: "view-refresh"
                onClicked: refresh()
                PlasmaComponents3.ToolTip { text: "Refresh" }
            }

            PlasmaComponents3.ToolButton {
                icon.name: "globe"
                onClicked: Qt.openUrlExternally(root.kimaiUrl)
                PlasmaComponents3.ToolTip { text: "Open Kimai" }
            }
        }

        Kirigami.Separator { Layout.fillWidth: true }

        // Entry list
        QQC2.ScrollView {
            Layout.fillWidth: true
            Layout.fillHeight: true

            ListView {
                id: entryList
                model: root.todayEntries
                spacing: 1
                clip: true

                delegate: Rectangle {
                    id: entryDelegate
                    required property var modelData
                    required property int index
                    width: entryList.width
                    height: entryRow.implicitHeight + Kirigami.Units.smallSpacing * 2
                    color: modelData.end === null ? Kirigami.Theme.positiveBackgroundColor
                                                  : "transparent"

                    RowLayout {
                        id: entryRow
                        anchors.fill: parent
                        anchors.margins: Kirigami.Units.smallSpacing
                        spacing: Kirigami.Units.smallSpacing

                        // Play / Stop button
                        PlasmaComponents3.ToolButton {
                            icon.name: entryDelegate.modelData.end === null
                                       ? "media-playback-stop" : "media-playback-start"
                            onClicked: {
                                if (entryDelegate.modelData.end === null) {
                                    stopTimesheet(entryDelegate.modelData.id)
                                } else {
                                    restartTimesheet(entryDelegate.modelData.id)
                                }
                            }
                            PlasmaComponents3.ToolTip {
                                text: entryDelegate.modelData.end === null ? "Stop" : "Start"
                            }
                        }

                        // Project & Activity info
                        ColumnLayout {
                            Layout.fillWidth: true
                            spacing: 0

                            PlasmaComponents3.Label {
                                text: entryProjectLabel(entryDelegate.modelData)
                                font.bold: true
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                            }
                            PlasmaComponents3.Label {
                                text: entryDelegate.modelData.description || ""
                                visible: text !== ""
                                elide: Text.ElideRight
                                Layout.fillWidth: true
                                opacity: 0.7
                                font.pixelSize: Kirigami.Theme.smallFont.pixelSize
                            }
                        }

                        // Duration
                        PlasmaComponents3.Label {
                            text: {
                                if (entryDelegate.modelData.end === null) {
                                    return formatDuration(root.elapsedSeconds)
                                }
                                return formatDuration(entryDelegate.modelData.duration)
                            }
                            font.bold: entryDelegate.modelData.end === null
                        }

                        // Delete button — first click arms, second confirms.
                        PlasmaComponents3.ToolButton {
                            id: deleteBtn
                            property bool armed: false
                            icon.name: armed ? "dialog-warning" : "edit-delete"
                            icon.color: armed ? "red" : undefined
                            onClicked: {
                                if (armed) {
                                    deleteTimesheet(entryDelegate.modelData.id)
                                    armed = false
                                } else {
                                    armed = true
                                    armTimer.restart()
                                }
                            }
                            PlasmaComponents3.ToolTip {
                                text: deleteBtn.armed ? "Click again to confirm delete"
                                                      : "Delete"
                            }
                            // Reset after 3 seconds if not confirmed.
                            Timer {
                                id: armTimer
                                interval: 3000
                                onTriggered: deleteBtn.armed = false
                            }
                        }
                    }
                }
            }
        }

        Kirigami.Separator { Layout.fillWidth: true }

        // New entry controls
        RowLayout {
            Layout.fillWidth: true
            Layout.margins: Kirigami.Units.smallSpacing
            spacing: Kirigami.Units.smallSpacing

            PlasmaComponents3.ComboBox {
                id: taskCombo
                Layout.fillWidth: true
                model: taskModel
                textRole: "label"
            }

            PlasmaComponents3.ToolButton {
                icon.name: "list-add"
                enabled: taskCombo.currentIndex >= 0
                onClicked: {
                    var task = taskModel.get(taskCombo.currentIndex)
                    startNewTimesheet(task.projectId, task.activityId)
                }
                PlasmaComponents3.ToolTip { text: "Start new timer" }
            }
        }
    }

    // ── Timers ─────────────────────────────────────────────────────

    // Poll the API every 30s.
    Timer {
        interval: 30000
        repeat: true
        running: true
        triggeredOnStart: true
        onTriggered: refresh()
    }

    // Tick the running timer every second.
    Timer {
        interval: 1000
        repeat: true
        running: root.isTracking
        onTriggered: root.elapsedSeconds += 1
    }

    // ── Init ───────────────────────────────────────────────────────

    Component.onCompleted: {
        if (tokenPlaceholder !== "@" + "KIMAI_API_TOKEN" + "@") {
            root.apiToken = tokenPlaceholder
        }
        refresh()
    }

    // ── Helpers ────────────────────────────────────────────────────

    function formatDuration(totalSeconds) {
        var h = Math.floor(totalSeconds / 3600)
        var m = Math.floor((totalSeconds % 3600) / 60)
        var pad = function(n) { return n < 10 ? "0" + n : "" + n }
        return h + ":" + pad(m)
    }

    function todayTotal() {
        var total = 0
        for (var i = 0; i < todayEntries.length; i++) {
            var e = todayEntries[i]
            if (e.end === null) {
                total += root.elapsedSeconds
            } else {
                total += e.duration
            }
        }
        return total
    }

    function entryProjectLabel(entry) {
        // The full=true response nests objects; the non-full uses IDs.
        var parts = []
        if (entry.project && typeof entry.project === "object") {
            if (entry.project.customer && entry.project.customer.name)
                parts.push(entry.project.customer.name)
            parts.push(entry.project.name)
        }
        if (entry.activity && typeof entry.activity === "object") {
            parts.push(entry.activity.name)
        }
        return parts.join(" › ") || "Timesheet #" + entry.id
    }

    // ── API calls ──────────────────────────────────────────────────

    function apiRequest(method, path, body, callback) {
        if (!root.apiToken) { root.hasError = true; return }
        var xhr = new XMLHttpRequest()
        xhr.open(method, root.kimaiUrl + path, true)
        xhr.setRequestHeader("Authorization", "Bearer " + root.apiToken)
        xhr.setRequestHeader("Content-Type", "application/json")
        xhr.onreadystatechange = function() {
            if (xhr.readyState !== XMLHttpRequest.DONE) return
            callback(xhr)
        }
        xhr.send(body ? JSON.stringify(body) : null)
    }

    function refresh() {
        fetchTodayEntries()
        fetchTasks()
    }

    function fetchTodayEntries() {
        var today = new Date()
        var begin = today.getFullYear() + "-"
                  + pad2(today.getMonth() + 1) + "-"
                  + pad2(today.getDate()) + "T00:00:00"
        apiRequest("GET", "/api/timesheets?begin=" + begin
                         + "&order=ASC&size=50&full=true", null,
            function(xhr) {
                if (xhr.status !== 200) { root.hasError = true; return }
                try {
                    var entries = JSON.parse(xhr.responseText)
                    root.hasError = false
                    root.todayEntries = entries
                    root.activeIndex = -1
                    root.isTracking = false
                    for (var i = 0; i < entries.length; i++) {
                        if (entries[i].end === null) {
                            root.activeIndex = i
                            root.isTracking = true
                            var beginDate = new Date(entries[i].begin)
                            var now = new Date()
                            root.elapsedSeconds = Math.floor(
                                (now.getTime() - beginDate.getTime()) / 1000)
                            break
                        }
                    }
                    if (!root.isTracking) root.elapsedSeconds = 0

                    // Remember the most recent project/activity for the
                    // "new entry" combos.  Entries are ASC-sorted so the
                    // last element is the newest.
                    if (entries.length > 0) {
                        var last = entries[entries.length - 1]
                        updateLastUsed(last)
                    } else {
                        fetchLastUsed()
                    }
                } catch (e) { root.hasError = true }
            })
    }

    function updateLastUsed(entry) {
        if (entry.project && typeof entry.project === "object")
            root.lastProjectId = entry.project.id
        else if (typeof entry.project === "number")
            root.lastProjectId = entry.project

        if (entry.activity && typeof entry.activity === "object")
            root.lastActivityId = entry.activity.id
        else if (typeof entry.activity === "number")
            root.lastActivityId = entry.activity
    }

    // When there are no entries today, look at the global history.
    function fetchLastUsed() {
        apiRequest("GET", "/api/timesheets/recent?size=1", null, function(xhr) {
            if (xhr.status !== 200) return
            try {
                var data = JSON.parse(xhr.responseText)
                if (data.length > 0) updateLastUsed(data[0])
            } catch (e) { /* ignore */ }
        })
    }

    function fetchTasks() {
        // Fetch projects first, then activities, and build a combined list.
        apiRequest("GET", "/api/projects?visible=1", null, function(pxhr) {
            if (pxhr.status !== 200) return
            try {
                var projects = JSON.parse(pxhr.responseText)
                // Map project ID → customer name
                var customerOf = {}
                for (var i = 0; i < projects.length; i++) {
                    customerOf[projects[i].id] = projects[i].parentTitle || ""
                }

                apiRequest("GET", "/api/activities?visible=1", null, function(axhr) {
                    if (axhr.status !== 200) return
                    try {
                        var activities = JSON.parse(axhr.responseText)
                        taskModel.clear()
                        var bestIdx = 0
                        for (var j = 0; j < activities.length; j++) {
                            var a = activities[j]
                            var pid = a.project
                            var parts = []
                            if (customerOf[pid]) parts.push(customerOf[pid])
                            if (a.parentTitle) parts.push(a.parentTitle)
                            parts.push(a.name)

                            taskModel.append({
                                projectId: pid,
                                activityId: a.id,
                                label: parts.join(" › ")
                            })
                            // Pre-select the last-used combination.
                            if (pid === root.lastProjectId &&
                                a.id === root.lastActivityId) {
                                bestIdx = j
                            }
                        }
                        if (taskModel.count > 0) {
                            taskCombo.currentIndex = bestIdx
                        }
                    } catch (e) { /* ignore */ }
                })
            } catch (e) { /* ignore */ }
        })
    }

    function stopTimesheet(id) {
        // QML XMLHttpRequest does not support PATCH; use GET which
        // Kimai also accepts for the stop/restart endpoints.
        apiRequest("GET", "/api/timesheets/" + id + "/stop", null, function(xhr) {
            fetchTodayEntries()
        })
    }

    function restartTimesheet(id) {
        apiRequest("GET", "/api/timesheets/" + id + "/restart", null, function(xhr) {
            fetchTodayEntries()
        })
    }

    function deleteTimesheet(id) {
        apiRequest("DELETE", "/api/timesheets/" + id, null, function(xhr) {
            fetchTodayEntries()
        })
    }

    function startNewTimesheet(projectId, activityId) {
        apiRequest("POST", "/api/timesheets",
                   { project: projectId, activity: activityId },
                   function(xhr) { fetchTodayEntries() })
    }

    function pad2(n) { return n < 10 ? "0" + n : "" + n }
}
