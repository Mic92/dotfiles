import Cocoa
import Foundation

// MARK: - Alertmanager API Types

struct Alert: Codable {
    let annotations: [String: String]?
    let labels: [String: String]
    let status: AlertStatus
    let generatorURL: String?
    let startsAt: String?
}

struct AlertStatus: Codable {
    let state: String
    let silencedBy: [String]
    let inhibitedBy: [String]
}

// MARK: - Menu Bar Controller

class AlertmanagerBarController: NSObject {
    private var statusItem: NSStatusItem!
    private var timer: Timer?
    private let alertmanagerURL: String
    private let pollInterval: TimeInterval
    private var lastAlerts: [Alert] = []

    init(url: String, interval: TimeInterval) {
        self.alertmanagerURL = url
        self.pollInterval = interval
        super.init()
    }

    func start() {
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.button?.action = #selector(openAlertmanager)
        statusItem.button?.target = self

        setStatus(text: "🔔 …", tooltip: "Loading alerts…")
        buildMenu(alerts: [])

        fetchAlerts()
        timer = Timer.scheduledTimer(
            withTimeInterval: pollInterval,
            repeats: true
        ) { [weak self] _ in
            self?.fetchAlerts()
        }
    }

    private func setStatus(text: String, tooltip: String) {
        DispatchQueue.main.async { [weak self] in
            self?.statusItem.button?.title = text
            self?.statusItem.button?.toolTip = tooltip
        }
    }

    @objc private func openAlertmanager() {
        if let url = URL(string: alertmanagerURL) {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func openURL(_ sender: NSMenuItem) {
        if let urlString = sender.representedObject as? String,
            let url = URL(string: urlString)
        {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func refreshNow(_ sender: NSMenuItem) {
        fetchAlerts()
    }

    // MARK: - Fetch Alerts

    private func fetchAlerts() {
        guard let url = URL(string: "\(alertmanagerURL)/api/v2/alerts") else {
            setStatus(text: "🔔 ⚠", tooltip: "Invalid URL")
            return
        }

        var request = URLRequest(url: url)
        request.timeoutInterval = 10

        URLSession.shared.dataTask(with: request) { [weak self] data, response, error in
            guard let self = self else { return }

            if let error = error {
                self.setStatus(text: "🔔 ?", tooltip: "Error: \(error.localizedDescription)")
                DispatchQueue.main.async {
                    self.buildMenu(alerts: [])
                }
                return
            }

            guard let data = data else {
                self.setStatus(text: "🔔 ?", tooltip: "No data received")
                return
            }

            do {
                let alerts = try JSONDecoder().decode([Alert].self, from: data)
                let active = alerts.filter { alert in
                    alert.status.state == "active"
                        && alert.status.silencedBy.isEmpty
                }
                self.lastAlerts = active

                let count = active.count
                let icon = count > 0 ? "🔴" : "✅"
                let tooltip =
                    count > 0
                    ? "\(count) active alert\(count == 1 ? "" : "s")"
                    : "All clear"

                self.setStatus(text: "\(icon) \(count)", tooltip: tooltip)
                DispatchQueue.main.async {
                    self.buildMenu(alerts: active)
                }
            } catch {
                self.setStatus(text: "🔔 ⚠", tooltip: "Parse error: \(error.localizedDescription)")
                DispatchQueue.main.async {
                    self.buildMenu(alerts: [])
                }
            }
        }.resume()
    }

    // MARK: - Build Menu

    private func buildMenu(alerts: [Alert]) {
        let menu = NSMenu()

        if alerts.isEmpty {
            let item = NSMenuItem(title: "No active alerts", action: nil, keyEquivalent: "")
            item.isEnabled = false
            menu.addItem(item)
        } else {
            // Group by alertname
            var grouped: [String: [Alert]] = [:]
            for alert in alerts {
                let name = alert.labels["alertname"] ?? "Unknown"
                grouped[name, default: []].append(alert)
            }

            for (name, group) in grouped.sorted(by: { $0.key < $1.key }) {
                // Section header
                let header = NSMenuItem(
                    title: "\(name) (\(group.count))",
                    action: nil,
                    keyEquivalent: ""
                )
                header.isEnabled = false
                let attrs: [NSAttributedString.Key: Any] = [
                    .font: NSFont.boldSystemFont(ofSize: 13)
                ]
                header.attributedTitle = NSAttributedString(string: header.title, attributes: attrs)
                menu.addItem(header)

                for alert in group.sorted(by: {
                    ($0.labels["host"] ?? "") < ($1.labels["host"] ?? "")
                }) {
                    let host = alert.labels["host"] ?? "unknown"
                    let desc =
                        alert.annotations?["description"]
                        ?? alert.labels["alertname"]
                        ?? "no description"
                    let short = desc.count > 80 ? String(desc.prefix(77)) + "…" : desc
                    let title = "  \(host): \(short)"

                    let item = NSMenuItem(
                        title: title,
                        action: #selector(openURL(_:)),
                        keyEquivalent: ""
                    )
                    item.target = self
                    item.representedObject = alert.generatorURL
                    item.toolTip = desc
                    menu.addItem(item)
                }

                menu.addItem(NSMenuItem.separator())
            }
        }

        menu.addItem(NSMenuItem.separator())

        let refreshItem = NSMenuItem(
            title: "Refresh Now",
            action: #selector(refreshNow(_:)),
            keyEquivalent: "r"
        )
        refreshItem.target = self
        menu.addItem(refreshItem)

        let openItem = NSMenuItem(
            title: "Open Alertmanager…",
            action: #selector(openAlertmanager),
            keyEquivalent: "o"
        )
        openItem.target = self
        menu.addItem(openItem)

        menu.addItem(NSMenuItem.separator())

        let quitItem = NSMenuItem(
            title: "Quit",
            action: #selector(NSApplication.terminate(_:)),
            keyEquivalent: "q"
        )
        menu.addItem(quitItem)

        statusItem.menu = menu
    }
}

// MARK: - Argument Parsing

func parseArgs() -> (url: String, interval: TimeInterval) {
    var url = "http://localhost:9093"
    var interval: TimeInterval = 300
    let args = CommandLine.arguments

    var i = 1
    while i < args.count {
        switch args[i] {
        case "--url":
            i += 1
            if i < args.count { url = args[i] }
        case "--interval":
            i += 1
            if i < args.count, let val = TimeInterval(args[i]) { interval = val }
        case "--help", "-h":
            fputs("""
                Usage: alertmanager-bar [OPTIONS]

                Options:
                  --url URL          Alertmanager base URL (default: http://localhost:9093)
                  --interval SECS    Poll interval in seconds (default: 300)
                  --help, -h         Show this help

                """, stderr)
            exit(0)
        default:
            fputs("Unknown option: \(args[i])\n", stderr)
            exit(1)
        }
        i += 1
    }
    return (url, interval)
}

// MARK: - Main

let config = parseArgs()

let app = NSApplication.shared
// Agent app: no dock icon, no main menu
app.setActivationPolicy(.accessory)

let controller = AlertmanagerBarController(url: config.url, interval: config.interval)
controller.start()

app.run()
