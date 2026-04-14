// nostr-chat-bar — macOS menubar frontend for nostr-chatd.
//
// Thin port of the noctalia-shell Panel.qml: same NDJSON-over-unix-socket
// protocol (see daemon/ipc.go), same "daemon is source of truth" model.
// We keep one persistent connection, send a `replay` on every connect,
// and mirror events into an NSTableView. Zero state survives the app —
// quit/relaunch just replays from sqlite.
//
// Deliberately Cocoa, not SwiftUI: single-file, no .app bundle, builds
// with bare `swiftc` like alertmanager-bar so the nix derivation stays
// trivial.

import Cocoa
import Carbon.HIToolbox
import Foundation
import Network

// MARK: - Wire types (mirror of daemon/ipc.go)

struct Msg: Codable {
    let id: String
    let pubkey: String?
    let content: String
    let ts: Int64
    let dir: String          // "in" / "out"
    let read: Bool?
    let ack: String?
    let image: String?
    let replyTo: String?
    let state: String?
}

struct Event: Codable {
    let kind: String
    let msg: Msg?
    let target: String?
    let mark: String?
    let image: String?
    let state: String?
    let tries: Int?
    let streaming: Bool?
    let relaysUp: Int?
    let relaysTotal: Int?
    let relays: [String]?
    let pubkey: String?
    let name: String?
    let unread: Int?
    let text: String?
}

// In-memory mirror — what the table view binds to.
struct Row {
    let id: String
    let mine: Bool
    var text: String
    let ts: Int64
    var ack: String
    var image: String
    var state: String
    var tries: Int
    let replyTo: String
}

// MARK: - Daemon socket

/// Persistent NDJSON unix socket. Auto-reconnects with capped backoff
/// and fires `onConnect` so the owner can issue a replay — that's the
/// whole resync protocol, identical to Main.qml's Loader<Socket>.
final class Daemon {
    var onEvent: ((Event) -> Void)?
    var onConnect: (() -> Void)?
    var onDisconnect: (() -> Void)?

    private let path: String
    private var conn: NWConnection?
    private var buf = Data()
    private var backoff: TimeInterval = 0.5
    private let q = DispatchQueue(label: "nostr-chatd.sock")

    init(path: String) { self.path = path }

    func start() { connect() }

    func send(_ obj: [String: Any]) {
        guard let c = conn, c.state == .ready,
              var data = try? JSONSerialization.data(withJSONObject: obj)
        else { return }
        data.append(0x0a)
        c.send(content: data, completion: .contentProcessed { _ in })
    }

    private func connect() {
        let ep = NWEndpoint.unix(path: path)
        let c = NWConnection(to: ep, using: .tcp)
        conn = c
        c.stateUpdateHandler = { [weak self] st in
            guard let self else { return }
            switch st {
            case .ready:
                self.backoff = 0.5
                DispatchQueue.main.async { self.onConnect?() }
                self.receive()
            case .failed, .cancelled:
                DispatchQueue.main.async { self.onDisconnect?() }
                self.scheduleReconnect()
            default: break
            }
        }
        c.start(queue: q)
    }

    private func scheduleReconnect() {
        // Both the state handler (.failed/.cancelled) and receive's
        // eof/err path can land here for the same connection — and
        // cancel() below itself triggers .cancelled. Gate on conn so
        // only the first caller schedules the timer.
        guard conn != nil else { return }
        conn?.cancel(); conn = nil; buf.removeAll()
        let d = backoff
        backoff = min(backoff * 2, 4.0)  // under daemon's RestartSec=5
        q.asyncAfter(deadline: .now() + d) { [weak self] in self?.connect() }
    }

    private func receive() {
        conn?.receive(minimumIncompleteLength: 1, maximumLength: 64 * 1024) {
            [weak self] data, _, eof, err in
            guard let self else { return }
            if let data, !data.isEmpty {
                self.buf.append(data)
                self.drain()
            }
            if eof || err != nil {
                DispatchQueue.main.async { self.onDisconnect?() }
                self.scheduleReconnect()
                return
            }
            self.receive()
        }
    }

    private func drain() {
        while let nl = buf.firstIndex(of: 0x0a) {
            let line = buf.subdata(in: buf.startIndex..<nl)
            buf.removeSubrange(buf.startIndex...nl)
            guard let ev = try? JSONDecoder().decode(Event.self, from: line) else {
                FileHandle.standardError.write(Data("bad ipc json: \(String(decoding: line, as: UTF8.self))\n".utf8))
                continue
            }
            DispatchQueue.main.async { self.onEvent?(ev) }
        }
    }
}

// MARK: - Drop-down panel
//
// Mimics noctalia's layer-shell card: borderless, floats above
// everything, pinned under the menubar of whichever screen the mouse
// is on, slides in/out. NSPanel + .nonactivatingPanel lets it grab key
// without stealing app focus from whatever's underneath — same UX as
// Spotlight.

final class DropPanel: NSPanel {
    // Borderless windows refuse key by default; override so the
    // TextView can actually receive typing.
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
    var onCancel: (() -> Void)?
    override func cancelOperation(_: Any?) { onCancel?() }  // Esc
}

final class ChatWindowController: NSWindowController, NSTableViewDataSource,
    NSTableViewDelegate, NSTextViewDelegate, NSSearchFieldDelegate
{
    private let daemon: Daemon
    private(set) var rows: [Row] = []
    private let maxHistory: Int

    private let table = NSTableView()
    private let scroll = NSScrollView()
    private let input = ComposeView()
    private let header = NSTextField(labelWithString: "Chat")
    private let status = NSTextField(labelWithString: "connecting…")
    private let dot = NSView()
    private let card = NSVisualEffectView()
    private var scrollHeight: NSLayoutConstraint!
    private let replyBar = NSTextField(labelWithString: "")
    private weak var replyRowRef: NSStackView?
    private let search = NSSearchField()
    private let searchCount = NSTextField(labelWithString: "")
    private weak var searchRowRef: NSStackView?
    // Hit list = row indices, newest-first so ↓ walks toward older.
    private var hits: [Int] = []
    private var hitCursor = 0
    private var replyTarget: Row? { didSet { updateReplyBar() } }

    private let panelWidth: CGFloat = 760
    private let panelHeight: CGFloat = 620

    var peerName = "Chat" { didSet { header.stringValue = peerName } }
    var onUnreadChanged: ((Int) -> Void)?
    private var unread = 0 { didSet { onUnreadChanged?(unread) } }

    init(daemon: Daemon, maxHistory: Int) {
        self.daemon = daemon
        self.maxHistory = maxHistory
        let w = DropPanel(
            contentRect: NSRect(x: 0, y: 0, width: 760, height: 620),
            styleMask: [.borderless, .nonactivatingPanel, .fullSizeContentView],
            backing: .buffered, defer: false)
        w.isReleasedWhenClosed = false
        w.isOpaque = false
        w.backgroundColor = .clear
        w.hasShadow = true
        w.level = .floating
        w.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        w.hidesOnDeactivate = false
        w.isMovable = false
        w.animationBehavior = .none  // we drive the slide ourselves
        super.init(window: w)
        w.onCancel = { [weak self] in self?.hide() }
        build()
    }
    required init?(coder: NSCoder) { fatalError() }

    // MARK: layout

    private func build() {
        guard let cv = window?.contentView else { return }

        // Rounded blurred card — the window itself stays clear so the
        // corner radius reads against whatever's behind it.
        card.material = .popover
        card.blendingMode = .behindWindow
        card.state = .active
        card.wantsLayer = true
        card.layer?.cornerRadius = 14
        card.layer?.maskedCorners = [.layerMinXMinYCorner, .layerMaxXMinYCorner]
        card.layer?.masksToBounds = true
        card.translatesAutoresizingMaskIntoConstraints = false
        cv.addSubview(card)
        NSLayoutConstraint.activate([
            card.leadingAnchor.constraint(equalTo: cv.leadingAnchor),
            card.trailingAnchor.constraint(equalTo: cv.trailingAnchor),
            card.topAnchor.constraint(equalTo: cv.topAnchor),
            card.bottomAnchor.constraint(equalTo: cv.bottomAnchor),
        ])

        header.font = .boldSystemFont(ofSize: 14)
        status.font = .systemFont(ofSize: 10)
        status.textColor = .secondaryLabelColor
        dot.wantsLayer = true
        dot.layer?.cornerRadius = 4
        dot.layer?.backgroundColor = NSColor.systemRed.cgColor

        let hair = NSBox(); hair.boxType = .separator

        let attach = iconButton("paperclip", #selector(pickFile))
        let send = iconButton("paperplane.fill", #selector(sendClicked))
        send.contentTintColor = .controlAccentColor
        send.keyEquivalent = "\r"
        send.keyEquivalentModifierMask = .command

        table.headerView = nil
        // Big-Sur+ tables default to .inset which pads the leading edge
        // and zero-pads trailing — our right-aligned bubbles end up
        // clipped against the scroller. .plain gives us the full row.
        table.style = .plain
        table.columnAutoresizingStyle = .lastColumnOnlyAutoresizingStyle
        table.rowSizeStyle = .custom
        table.usesAutomaticRowHeights = true
        table.selectionHighlightStyle = .none
        table.backgroundColor = .clear
        table.intercellSpacing = NSSize(width: 0, height: 8)
        let col = NSTableColumn(identifier: .init("c"))
        col.resizingMask = .autoresizingMask
        table.addTableColumn(col)
        table.dataSource = self
        table.delegate = self
        scroll.documentView = table
        scroll.hasVerticalScroller = true
        scroll.drawsBackground = false

        input.font = .systemFont(ofSize: 13)
        input.isRichText = false
        input.delegate = self
        input.isAutomaticQuoteSubstitutionEnabled = false
        input.drawsBackground = false
        // The pill backdrop is always dark (textBackgroundColor at 60%
        // alpha over a .popover blur), independent of system theme. Pin
        // the text view to darkAqua so the semantic colours it derives
        // — textColor, insertionPoint, selection — resolve to light
        // values without us re-seeding typingAttributes after every
        // string mutation.
        input.appearance = NSAppearance(named: .darkAqua)
        input.textColor = .labelColor
        input.insertionPointColor = .labelColor
        input.textContainerInset = NSSize(width: 8, height: 10)
        // Bare NSTextView() ships a 0×0 textContainer that doesn't
        // track the view — glyphs lay out into nothing while the caret
        // still advances, which is the "invisible text" symptom. Wire
        // up the bits NSTextView.scrollableTextView() would have set.
        let huge = CGFloat.greatestFiniteMagnitude
        input.minSize = .zero
        input.maxSize = NSSize(width: huge, height: huge)
        input.isVerticallyResizable = true
        input.isHorizontallyResizable = false
        input.autoresizingMask = .width
        input.textContainer?.widthTracksTextView = true
        input.textContainer?.containerSize = NSSize(width: 0, height: huge)
        input.onSend = { [weak self] in self?.doSend() }
        input.onImagePaste = { [weak self] path in
            self?.daemon.send(["cmd": "send-file", "path": path, "unlink": true])
        }
        let inScroll = NSScrollView()
        inScroll.documentView = input
        inScroll.hasVerticalScroller = true
        inScroll.drawsBackground = false
        inScroll.borderType = .noBorder
        // Rounded pill around the compose box — NSBox would draw its
        // own title/stroke, a plain layer-backed view is simpler and
        // we can tint it to read against the blur.
        let pill = NSView()
        pill.wantsLayer = true
        pill.layer?.cornerRadius = 18
        pill.layer?.backgroundColor = NSColor.textBackgroundColor.withAlphaComponent(0.6).cgColor
        pill.layer?.borderWidth = 1
        pill.layer?.borderColor = NSColor.separatorColor.cgColor
        inScroll.translatesAutoresizingMaskIntoConstraints = false
        pill.addSubview(inScroll)
        NSLayoutConstraint.activate([
            inScroll.leadingAnchor.constraint(equalTo: pill.leadingAnchor, constant: 6),
            inScroll.trailingAnchor.constraint(equalTo: pill.trailingAnchor, constant: -6),
            inScroll.topAnchor.constraint(equalTo: pill.topAnchor),
            inScroll.bottomAnchor.constraint(equalTo: pill.bottomAnchor),
        ])

        // ── Search bar (⌘F) ───────────────────────────────────────────
        // Substring match over the in-memory mirror; outlines via
        // BubbleCell.searchHit, ↵/⇧↵ step, Esc closes. Cheap enough
        // at maxHistory rows that we just reloadData on every change
        // — no async, no diffing.
        search.placeholderString = "Search"
        search.sendsSearchStringImmediately = true
        search.target = self
        search.action = #selector(searchChanged)
        search.delegate = self
        searchCount.font = .monospacedDigitSystemFont(ofSize: 11, weight: .regular)
        searchCount.textColor = .secondaryLabelColor
        let prev = iconButton("chevron.up", #selector(searchPrev), size: 22, pill: false)
        let next = iconButton("chevron.down", #selector(searchNext), size: 22, pill: false)
        let close = iconButton("xmark", #selector(searchClose), size: 22, pill: false)
        let searchRow = NSStackView(views: [search, searchCount, prev, next, close])
        searchRow.orientation = .horizontal
        searchRow.alignment = .centerY
        searchRow.spacing = 4
        searchRow.isHidden = true
        searchRowRef = searchRow

        let find = iconButton("magnifyingglass", #selector(searchToggle),
                              size: 22, pill: false)
        let headRow = NSStackView(views: [header, status, NSView(), find, dot])
        headRow.orientation = .horizontal
        headRow.alignment = .centerY
        dot.widthAnchor.constraint(equalToConstant: 8).isActive = true
        dot.heightAnchor.constraint(equalToConstant: 8).isActive = true

        // ↳-reply context bar above the pill, hidden until set.
        replyBar.font = .systemFont(ofSize: 11)
        replyBar.textColor = .secondaryLabelColor
        replyBar.lineBreakMode = .byTruncatingTail
        replyBar.setContentHuggingPriority(.defaultLow, for: .horizontal)
        let clear = NSButton(
            image: NSImage(systemSymbolName: "xmark.circle.fill",
                           accessibilityDescription: "clear")!,
            target: self, action: #selector(clearReply))
        clear.isBordered = false
        clear.contentTintColor = .secondaryLabelColor
        let replyRow = NSStackView(views: [replyBar, clear])
        replyRow.orientation = .horizontal
        replyRow.alignment = .centerY
        replyRow.spacing = 6
        replyRow.isHidden = true
        replyRowRef = replyRow

        let sendRow = NSStackView(views: [pill, attach, send])
        sendRow.orientation = .horizontal
        sendRow.alignment = .centerY
        sendRow.spacing = 10
        pill.heightAnchor.constraint(equalToConstant: 38).isActive = true

        // Bottom-anchor short conversations: NSClipView clamps bounds
        // to (0,0) when the document is smaller than the viewport, so
        // contentInsets can't push rows down. Instead let the scroll
        // view itself shrink to fit and put the stretch above it.
        let historyBox = NSView()
        scroll.translatesAutoresizingMaskIntoConstraints = false
        historyBox.addSubview(scroll)
        scrollHeight = scroll.heightAnchor.constraint(equalToConstant: 0)
        scrollHeight.priority = .defaultHigh  // yield once content overflows
        NSLayoutConstraint.activate([
            scroll.leadingAnchor.constraint(equalTo: historyBox.leadingAnchor),
            scroll.trailingAnchor.constraint(equalTo: historyBox.trailingAnchor),
            scroll.bottomAnchor.constraint(equalTo: historyBox.bottomAnchor),
            scroll.topAnchor.constraint(greaterThanOrEqualTo: historyBox.topAnchor),
            scrollHeight,
        ])
        table.postsFrameChangedNotifications = true
        NotificationCenter.default.addObserver(
            self, selector: #selector(anchorBottom),
            name: NSView.frameDidChangeNotification, object: table)

        let root = NSStackView(views: [headRow, searchRow, hair, historyBox, replyRow, sendRow])
        root.orientation = .vertical
        root.spacing = 10
        root.edgeInsets = NSEdgeInsets(top: 14, left: 16, bottom: 16, right: 16)
        root.translatesAutoresizingMaskIntoConstraints = false
        card.addSubview(root)
        NSLayoutConstraint.activate([
            root.leadingAnchor.constraint(equalTo: card.leadingAnchor),
            root.trailingAnchor.constraint(equalTo: card.trailingAnchor),
            root.topAnchor.constraint(equalTo: card.topAnchor),
            root.bottomAnchor.constraint(equalTo: card.bottomAnchor),
        ])
    }

    // Round SF-Symbol button — matches the 📎/✈︎ pair in noctalia
    // without pulling in a whole button style.
    private func iconButton(_ symbol: String, _ action: Selector,
                            size: CGFloat = 32, pill: Bool = true) -> NSButton {
        let b = NSButton(image: NSImage(systemSymbolName: symbol, accessibilityDescription: nil)
            ?? NSImage(), target: self, action: action)
        b.isBordered = false
        b.bezelStyle = .regularSquare
        b.imageScaling = .scaleProportionallyUpOrDown
        b.symbolConfiguration = .init(pointSize: size * 0.5, weight: .regular)
        b.contentTintColor = .secondaryLabelColor
        b.widthAnchor.constraint(equalToConstant: size).isActive = true
        b.heightAnchor.constraint(equalToConstant: size).isActive = true
        b.wantsLayer = true
        b.layer?.cornerRadius = size / 2
        if pill {
            b.layer?.backgroundColor =
                NSColor.textBackgroundColor.withAlphaComponent(0.5).cgColor
        }
        return b
    }

    // Track table height into the scroll-view's preferred height so
    // a short conversation hugs the compose box. Once it exceeds the
    // slot the greaterThanOrEqual top constraint wins and it scrolls.
    @objc private func anchorBottom() {
        scrollHeight.constant = table.bounds.height
    }

    // MARK: state

    func setRelays(streaming: Bool, up: Int, total: Int, urls: [String]) {
        status.stringValue = streaming ? "connected · \(up)/\(total)" : "offline"
        status.toolTip = urls.joined(separator: "\n")
        let c: NSColor = !streaming ? .systemRed : (up < total ? .systemOrange : .systemGreen)
        dot.layer?.backgroundColor = c.cgColor
    }

    func apply(_ ev: Event) {
        switch ev.kind {
        case "status":
            if let n = ev.name, !n.isEmpty { peerName = n }
            setRelays(streaming: ev.streaming ?? false,
                      up: ev.relaysUp ?? 0, total: ev.relaysTotal ?? 0,
                      urls: ev.relays ?? [])
            if let u = ev.unread { unread = u }
        case "msg":
            guard let m = ev.msg else { return }
            insert(m)
        case "sent":
            guard let id = ev.target else { return }
            if ev.state == "cancelled" {
                rows.removeAll { $0.id == id }; table.reloadData()
            } else { patch(id) { $0.state = "sent"; $0.tries = 0 } }
        case "retry":
            guard let id = ev.target else { return }
            patch(id) { $0.tries = ev.tries ?? 0 }
        case "ack":
            guard let id = ev.target else { return }
            patch(id) { $0.ack = ev.mark ?? "✓" }
        case "img":
            guard let id = ev.target else { return }
            patch(id) { $0.image = ev.image ?? "" }
        case "error":
            status.stringValue = ev.text ?? "error"
            status.textColor = .systemRed
            DispatchQueue.main.asyncAfter(deadline: .now() + 10) { [weak self] in
                self?.status.textColor = .secondaryLabelColor
            }
        default: break
        }
    }

    private func insert(_ m: Msg) {
        if rows.contains(where: { $0.id == m.id }) { return }
        let r = Row(id: m.id, mine: m.dir == "out", text: m.content, ts: m.ts,
                    ack: m.ack ?? "", image: m.image ?? "",
                    state: m.state ?? "sent", tries: 0,
                    replyTo: m.replyTo ?? "")
        // insert-sort by ts — replay and live interleave
        var i = rows.count
        while i > 0 && rows[i-1].ts > r.ts { i -= 1 }
        rows.insert(r, at: i)
        if rows.count > maxHistory { rows.removeFirst(rows.count - maxHistory) }
        table.reloadData()
        if atBottom || r.mine { scrollToEnd() }
        if m.dir == "in" && !(m.read ?? true) {
            unread += 1
            if !(window?.isVisible ?? false) { notify(m.content) }
        }
    }

    func refreshTimestamps() {
        guard window?.isVisible == true, !rows.isEmpty else { return }
        table.reloadData(
            forRowIndexes: IndexSet(integersIn: 0..<rows.count),
            columnIndexes: [0])
    }

    private func patch(_ id: String, _ f: (inout Row) -> Void) {
        guard let i = rows.firstIndex(where: { $0.id == id }) else { return }
        f(&rows[i])
        table.reloadData(forRowIndexes: [i], columnIndexes: [0])
    }

    private var atBottom: Bool {
        let clip = scroll.contentView.bounds
        return clip.maxY >= table.bounds.height - 40
    }
    private func scrollToEnd() {
        guard rows.count > 0 else { return }
        table.scrollRowToVisible(rows.count - 1)
    }

    // UNUserNotificationCenter refuses to run from a bare binary (no
    // Info.plist → "bundleProxyForCurrentProcess is nil" abort). The
    // deprecated NSUserNotification path still works without a bundle
    // id, so use it until this ships as a proper .app.
    @available(macOS, deprecated: 11.0)
    private func notify(_ body: String) {
        let n = NSUserNotification()
        n.title = peerName
        n.informativeText = body.count > 120 ? String(body.prefix(117)) + "…" : body
        NSUserNotificationCenter.default.deliver(n)
    }

    // MARK: actions

    func toggle() {
        guard let w = window else { return }
        if w.isVisible { hide() } else { present() }
    }

    // Target rect: centred under the menubar of the screen that
    // currently holds the mouse — same "appear where I'm looking"
    // rule as the noctalia panel's withCurrentScreen.
    private func targetFrame() -> NSRect {
        let mouse = NSEvent.mouseLocation
        let screen = NSScreen.screens.first { $0.frame.contains(mouse) }
            ?? NSScreen.main ?? NSScreen.screens[0]
        let vis = screen.visibleFrame
        let w = min(panelWidth, vis.width - 40)
        let h = min(panelHeight, vis.height - 40)
        let x = vis.midX - w / 2
        let y = vis.maxY - h  // visibleFrame already excludes the menubar
        return NSRect(x: x, y: y, width: w, height: h)
    }

    // Animate the card layer, not the window frame: NSVisualEffectView
    // samples the backdrop at the window's on-screen rect, so moving
    // the window mid-animation makes the blur crawl. Parking the
    // window at its final frame and sliding the layer keeps the blur
    // pinned and lets us use a CASpringAnimation for the overshoot
    // that reads as "dropped in", which the linear window animator
    // can't do.
    func present() {
        guard let w = window, let layer = card.layer else { return }
        w.setFrame(targetFrame(), display: false)
        w.alphaValue = 1
        // .accessory apps need an explicit activate or the panel comes
        // up keyless and the first keystroke goes to the app behind.
        NSApp.activate(ignoringOtherApps: true)
        w.makeKeyAndOrderFront(nil)

        // AppKit on non-flipped superviews sets the layer geometry
        // flipped — +y is up — so "above the menubar" is +height.
        let off = w.frame.height
        let spring = CASpringAnimation(keyPath: "transform.translation.y")
        spring.fromValue = off
        spring.toValue = 0
        spring.damping = 28
        spring.stiffness = 320
        spring.mass = 0.9
        spring.initialVelocity = 4
        spring.duration = spring.settlingDuration
        let fade = CABasicAnimation(keyPath: "opacity")
        fade.fromValue = 0
        fade.toValue = 1
        fade.duration = 0.12
        // Seed the model layer at the destination before adding the
        // animation — avoids a one-frame flash at y=0 if CA commits
        // between orderFront and add().
        layer.transform = CATransform3DIdentity
        layer.removeAllAnimations()
        layer.add(spring, forKey: "drop")
        layer.add(fade, forKey: "fade")
        w.invalidateShadow()

        w.makeFirstResponder(input)
        scrollToEnd()
        unread = 0
        daemon.send(["cmd": "mark-read"])
    }

    func hide() {
        guard let w = window, w.isVisible, let layer = card.layer else { return }
        let off = w.frame.height
        CATransaction.begin()
        CATransaction.setCompletionBlock {
            w.orderOut(nil)
            layer.transform = CATransform3DIdentity
            // present() called NSApp.activate to grab key for the
            // .accessory process; orderOut alone leaves us active with
            // zero windows, so the previously-frontmost app stays
            // unfocused. NSApp.hide hands activation back to it.
            if NSApp.isActive { NSApp.hide(nil) }
        }
        let slide = CABasicAnimation(keyPath: "transform.translation.y")
        slide.toValue = off
        slide.duration = 0.16
        slide.timingFunction = CAMediaTimingFunction(name: .easeIn)
        slide.fillMode = .forwards
        slide.isRemovedOnCompletion = false
        layer.add(slide, forKey: "drop")
        // Fade the window, not the layer — takes the shadow with it so
        // there's no orphaned drop-shadow rectangle for 160ms.
        w.animator().alphaValue = 0
        CATransaction.commit()
    }

    @objc private func sendClicked() { doSend() }
    private func doSend() {
        let t = input.string.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !t.isEmpty else { return }
        var c: [String: Any] = ["cmd": "send", "text": t]
        if let r = replyTarget { c["replyTo"] = r.id }
        daemon.send(c)
        input.string = ""
        replyTarget = nil
    }
    @objc private func clearReply() { replyTarget = nil }

    // MARK: search

    @objc func searchToggle() {
        if searchRowRef?.isHidden == false { searchClose(); return }
        searchRowRef?.isHidden = false
        window?.makeFirstResponder(search)
        searchChanged()
    }
    @objc private func searchClose() {
        search.stringValue = ""
        searchRowRef?.isHidden = true
        hits = []; hitCursor = 0
        table.reloadData()
        window?.makeFirstResponder(input)
    }
    @objc private func searchChanged() {
        let q = search.stringValue.lowercased()
        if q.isEmpty {
            hits = []; hitCursor = 0; searchCount.stringValue = ""
            table.reloadData(); return
        }
        hits = rows.indices.reversed().filter {
            rows[$0].text.lowercased().contains(q)
        }
        hitCursor = 0
        table.reloadData()
        jump()
    }
    @objc private func searchPrev() { step(-1) }
    @objc private func searchNext() { step(1) }
    private func step(_ d: Int) {
        guard !hits.isEmpty else { return }
        hitCursor = (hitCursor + d + hits.count) % hits.count
        // Cell already configured; only the "current" outline moves.
        table.reloadData()
        jump()
    }
    private func jump() {
        searchCount.stringValue = hits.isEmpty ? "0"
            : "\(hitCursor + 1)/\(hits.count)"
        if let i = hits[safe: hitCursor] {
            table.scrollRowToVisible(i)
        }
    }
    private func updateReplyBar() {
        let on = replyTarget != nil
        replyRowRef?.isHidden = !on
        replyBar.stringValue = on ? "↳ " + snippet(replyTarget!.text, 80) : ""
        if on { window?.makeFirstResponder(input) }
    }
    private func snippet(_ s: String, _ n: Int) -> String {
        let t = s.replacingOccurrences(of: "\n", with: " ")
        return t.count > n ? String(t.prefix(n - 1)) + "…" : t
    }
    @objc private func pickFile() {
        let p = NSOpenPanel()
        p.allowedContentTypes = [.image]
        p.allowsMultipleSelection = false
        // Our panel sits at .floating; a free-standing NSOpenPanel
        // opens at .normal and ends up underneath. Either present as a
        // sheet on our window or temporarily lift its level — the
        // sheet keeps the chat visible behind it and returns focus to
        // the input on dismiss without extra bookkeeping.
        guard let w = window else { return }
        p.beginSheetModal(for: w) { [weak self] r in
            guard r == .OK, let u = p.url else { return }
            self?.daemon.send(["cmd": "send-file", "path": u.path])
        }
    }

    // Enter sends, Shift+Enter newlines — chat-app convention, matches
    // Panel.qml's handleReturn.
    func textView(_: NSTextView, doCommandBy sel: Selector) -> Bool {
        // Return handling lives in ComposeView.keyDown; only Esc here.
        guard sel == #selector(NSResponder.cancelOperation(_:)) else { return false }
        if replyTarget != nil { replyTarget = nil } else { hide() }
        return true
    }

    // NSSearchField key handling — separate delegate hook from the
    // NSTextView one above. ↵/⇧↵ step, Esc closes, ↓/↑ step too.
    func control(_: NSControl, textView _: NSTextView,
                 doCommandBy sel: Selector) -> Bool {
        switch sel {
        case #selector(NSResponder.insertNewline(_:)),
             #selector(NSResponder.moveDown(_:)):
            step(1); return true
        case #selector(NSResponder.insertBacktab(_:)),
             #selector(NSResponder.moveUp(_:)):
            step(-1); return true
        case #selector(NSResponder.cancelOperation(_:)):
            searchClose(); return true
        default: return false
        }
    }

    // MARK: table

    func numberOfRows(in _: NSTableView) -> Int { rows.count }

    func tableView(_ tv: NSTableView, viewFor _: NSTableColumn?, row: Int) -> NSView? {
        let r = rows[row]
        let cell = tv.makeView(withIdentifier: .init("bubble"), owner: nil) as? BubbleCell
            ?? BubbleCell()
        cell.identifier = .init("bubble")
        cell.onReply = { [weak self] in self?.replyTarget = r }
        let q = r.replyTo.isEmpty ? nil
            : rows.first(where: { $0.id == r.replyTo })?.text
        let sq = (searchRowRef?.isHidden ?? true) ? "" : search.stringValue
        cell.configure(r, ago: ago(r.ts), quoted: q,
                       searchHit: !sq.isEmpty
                           && r.text.lowercased().contains(sq.lowercased()),
                       searchCurrent: hits[safe: hitCursor] == row)
        return cell
    }

    private let rel: RelativeDateTimeFormatter = {
        let f = RelativeDateTimeFormatter(); f.unitsStyle = .short; return f
    }()
    private func ago(_ ts: Int64) -> String {
        let d = Date(timeIntervalSince1970: TimeInterval(ts))
        if Date().timeIntervalSince(d) < 60 { return "now" }
        return rel.localizedString(for: d, relativeTo: Date())
    }
}

/// NSTextView subclass so paste interception sits in the responder
/// chain — a `paste:` on the window controller never fires because the
/// text view handles it first.
final class ComposeView: NSTextView {
    var onImagePaste: ((String) -> Void)?

    var onSend: (() -> Void)?

    // Handle Return here, before key-binding resolution —
    // StandardKeyBinding.dict has no Shift+Return entry, so by the
    // time doCommandBy fires the modifier is already lost. Any
    // modifier means "newline", bare Return means "send".
    override func keyDown(with e: NSEvent) {
        guard e.keyCode == 36 || e.keyCode == 76 else {  // Return / Enter
            super.keyDown(with: e); return
        }
        if e.modifierFlags.isDisjoint(with: [.shift, .option, .control]) {
            onSend?()
        } else {
            insertNewlineIgnoringFieldEditor(nil)
        }
    }
    override func paste(_ sender: Any?) {
        let pb = NSPasteboard.general
        if pb.string(forType: .string) != nil { super.paste(sender); return }
        guard let img = NSImage(pasteboard: pb),
              let tiff = img.tiffRepresentation,
              let rep = NSBitmapImageRep(data: tiff),
              let png = rep.representation(using: .png, properties: [:])
        else { return }
        let f = (NSTemporaryDirectory() as NSString)
            .appendingPathComponent("nostr-chat-paste-\(Int(Date().timeIntervalSince1970)).png")
        try? png.write(to: URL(fileURLWithPath: f))
        onImagePaste?(f)
    }
}

// MARK: - Bubble cell

final class BubbleCell: NSTableCellView {
    private let bubble = NSView()
    private let text = NSTextField(wrappingLabelWithString: "")
    private let meta = NSTextField(labelWithString: "")
    private let quote = NSTextField(labelWithString: "")
    private let img = NSImageView()
    private let reply = NSButton()
    var onReply: (() -> Void)?
    private var lead: NSLayoutConstraint!
    private var trail: NSLayoutConstraint!
    private var replyLead: NSLayoutConstraint!
    private var replyTrail: NSLayoutConstraint!

    override init(frame f: NSRect) { super.init(frame: f); build() }
    required init?(coder: NSCoder) { fatalError() }

    private let stack = NSStackView()

    private func build() {
        bubble.wantsLayer = true
        bubble.layer?.cornerRadius = 12
        // allowsEditingTextAttributes is what makes NSTextField (a)
        // honour .link attributes for click/hover and (b) hand the
        // *attributed* string to the shared field editor on selection
        // — without it, clicking the label re-renders via stringValue
        // and the markdown styling vanishes. The field stays
        // non-editable; the flag name is a historical misnomer.
        text.isSelectable = true
        text.allowsEditingTextAttributes = true
        text.font = .systemFont(ofSize: 13)
        text.drawsBackground = false
        meta.font = .systemFont(ofSize: 9)
        meta.textColor = .tertiaryLabelColor
        quote.font = .systemFont(ofSize: 11)
        quote.lineBreakMode = .byTruncatingTail
        quote.isHidden = true
        reply.isBordered = false
        reply.bezelStyle = .regularSquare
        reply.contentTintColor = .secondaryLabelColor
        reply.target = self
        reply.action = #selector(replyClicked)
        reply.alphaValue = 0
        reply.translatesAutoresizingMaskIntoConstraints = false
        img.imageScaling = .scaleProportionallyUpOrDown
        img.wantsLayer = true
        img.layer?.cornerRadius = 8
        img.layer?.masksToBounds = true

        for v in [quote, img, text, meta] { stack.addArrangedSubview(v) }
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 4
        stack.translatesAutoresizingMaskIntoConstraints = false
        bubble.translatesAutoresizingMaskIntoConstraints = false
        bubble.addSubview(stack)
        addSubview(bubble)
        addSubview(reply)
        // Hover-reveal in the gutter beside the bubble — same UX as the
        // QML version. NSTrackingArea on the row toggles alpha.
        let track = NSTrackingArea(
            rect: .zero,
            options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
            owner: self, userInfo: nil)
        addTrackingArea(track)

        lead = bubble.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 6)
        trail = bubble.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -6)
        replyLead = reply.leadingAnchor.constraint(equalTo: bubble.trailingAnchor, constant: 6)
        replyTrail = reply.trailingAnchor.constraint(equalTo: bubble.leadingAnchor, constant: -6)
        // Padding via explicit constants instead of stack.edgeInsets:
        // NSStackView only honours edgeInsets along its own axis; the
        // perpendicular alignment guide it pins children to is the
        // stack's raw edge, so the right inset silently collapsed to 0
        // on .trailing-aligned ("mine") bubbles.
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: bubble.leadingAnchor, constant: 14),
            stack.trailingAnchor.constraint(equalTo: bubble.trailingAnchor, constant: -14),
            stack.topAnchor.constraint(equalTo: bubble.topAnchor, constant: 10),
            stack.bottomAnchor.constraint(equalTo: bubble.bottomAnchor, constant: -8),
            bubble.topAnchor.constraint(equalTo: topAnchor),
            bubble.bottomAnchor.constraint(equalTo: bottomAnchor),
            bubble.widthAnchor.constraint(lessThanOrEqualTo: widthAnchor, multiplier: 0.78),
            img.widthAnchor.constraint(lessThanOrEqualToConstant: 320),
            img.heightAnchor.constraint(lessThanOrEqualToConstant: 240),
            reply.centerYAnchor.constraint(equalTo: bubble.centerYAnchor),
            reply.widthAnchor.constraint(equalToConstant: 20),
        ])
    }

    @objc private func replyClicked() { onReply?() }
    override func mouseEntered(with _: NSEvent) { reply.animator().alphaValue = 1 }
    override func mouseExited(with _: NSEvent) { reply.animator().alphaValue = 0 }

    func configure(_ r: Row, ago: String, quoted: String?,
                   searchHit: Bool = false, searchCurrent: Bool = false) {
        bubble.layer?.borderWidth = searchHit ? 2 : 0
        bubble.layer?.borderColor = (searchCurrent
            ? NSColor.systemYellow : NSColor.systemOrange).cgColor
        // Deactivate both first — flipping one before the other can
        // briefly leave lead+trail active on a reused cell, which
        // AppKit logs as an unsatisfiable-constraint storm.
        lead.isActive = false; trail.isActive = false
        replyLead.isActive = false; replyTrail.isActive = false
        lead.isActive = !r.mine; trail.isActive = r.mine
        replyLead.isActive = !r.mine; replyTrail.isActive = r.mine
        // Arrow points toward the bubble it sits beside, so mirror for
        // own messages (button is in the left gutter → point right).
        reply.image = NSImage(
            systemSymbolName: r.mine ? "arrowshape.turn.up.right"
                                     : "arrowshape.turn.up.left",
            accessibilityDescription: "reply")
        stack.alignment = r.mine ? .trailing : .leading
        quote.isHidden = quoted == nil && r.replyTo.isEmpty
        quote.textColor = r.mine
            ? NSColor.white.withAlphaComponent(0.7) : .secondaryLabelColor
        quote.stringValue = "↳ " + (quoted.map {
            $0.count > 60 ? String($0.prefix(59)) + "…" : $0
        } ?? "…")
        bubble.layer?.backgroundColor = (r.mine
            ? NSColor.controlAccentColor
            : NSColor.textBackgroundColor.withAlphaComponent(0.7)).cgColor
        text.textColor = r.mine ? .white : .labelColor
        meta.textColor = r.mine
            ? NSColor.white.withAlphaComponent(0.7) : .tertiaryLabelColor
        // Render markdown when the runtime supports it (macOS 12+);
        // fall back to plain text so the bubble never shows raw ** **.
        if let attr = try? NSAttributedString(
            markdown: r.text,
            options: .init(interpretedSyntax: .inlineOnlyPreservingWhitespace))
        {
            let base = text.font!
            let mut = NSMutableAttributedString(attributedString: attr)
            let all = NSRange(location: 0, length: mut.length)
            mut.addAttribute(.foregroundColor, value: text.textColor!, range: all)
            // Tint links so they read on the accent-colour bubble; the
            // field editor would otherwise re-apply NSColor.linkColor.
            let linkTint: NSColor = r.mine
                ? .white.withAlphaComponent(0.95) : .linkColor
            mut.enumerateAttribute(.link, in: all) { v, range, _ in
                guard v != nil else { return }
                mut.addAttributes([
                    .foregroundColor: linkTint,
                    .underlineStyle: NSUnderlineStyle.single.rawValue,
                    .cursor: NSCursor.pointingHand,
                ], range: range)
            }
            // The markdown parser encodes **bold**/`code` as font
            // traits on its own SF default; resize to ours without
            // flattening those traits back to regular.
            mut.enumerateAttribute(.font, in: all) { v, range, _ in
                let f = (v as? NSFont) ?? base
                let sized = NSFontManager.shared.convert(f, toSize: base.pointSize)
                mut.addAttribute(.font, value: sized, range: range)
            }
            text.attributedStringValue = mut
        } else {
            text.stringValue = r.text
        }
        var m = ago
        if r.mine {
            if r.tries > 0 { m += "  ⚠" }
            else if r.state == "pending" { m += "  …" }
            else if !r.ack.isEmpty { m += "  \(r.ack)" }
        }
        meta.stringValue = m
        if !r.image.isEmpty, let i = NSImage(contentsOfFile: r.image) {
            img.image = i; img.isHidden = false
        } else { img.image = nil; img.isHidden = true }
    }
}

// MARK: - App

final class AppController: NSObject, NSApplicationDelegate {
    private let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
    private let daemon: Daemon
    private let chat: ChatWindowController

    private let maxHistory: Int
    private var hotkey: EventHotKeyRef?

    init(socket: String, maxHistory: Int) {
        self.maxHistory = maxHistory
        daemon = Daemon(path: socket)
        chat = ChatWindowController(daemon: daemon, maxHistory: maxHistory)
        super.init()
    }

    func applicationDidFinishLaunching(_: Notification) {
        item.button?.title = "💬"
        registerHotkey()
        registerFind()
        item.button?.action = #selector(statusClicked(_:))
        item.button?.target = self
        item.button?.sendAction(on: [.leftMouseUp, .rightMouseUp])

        chat.onUnreadChanged = { [weak self] n in
            self?.item.button?.title = n > 0 ? "💬 \(n)" : "💬"
        }

        daemon.onEvent = { [weak self] ev in self?.chat.apply(ev) }
        daemon.onConnect = { [weak self] in
            guard let self else { return }
            self.daemon.send(["cmd": "replay", "n": self.maxHistory])
        }
        daemon.onDisconnect = { [weak self] in
            self?.chat.setRelays(streaming: false, up: 0, total: 0, urls: [])
        }
        daemon.start()

        // Relative timestamps ("now" → "1m") need a periodic redraw;
        // a coarse minute tick is enough and only runs while visible.
        Timer.scheduledTimer(withTimeInterval: 30, repeats: true) { [weak self] _ in
            self?.chat.refreshTimestamps()
        }
    }

    @objc private func statusClicked(_ sender: NSStatusBarButton) {
        guard let ev = NSApp.currentEvent else { return }
        if ev.type == .rightMouseUp {
            let m = NSMenu()
            m.addItem(withTitle: "Open \(chat.peerName)", action: #selector(open), keyEquivalent: "")
                .target = self
            m.addItem(.separator())
            m.addItem(withTitle: "Quit", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
            item.menu = m
            item.button?.performClick(nil)
            item.menu = nil
        } else {
            chat.toggle()
        }
    }
    @objc private func open() { chat.present() }

    // ⌥G global toggle. Carbon RegisterEventHotKey is the one API that
    // grabs a system-wide key without Accessibility permissions or an
    // app bundle — CGEventTap and NSEvent.addGlobalMonitor both need
    // the TCC prompt, which a bare nix binary can't satisfy.
    // ⌘F while the panel is up. The compose NSTextView would otherwise
    // beep on performFindPanelAction:. addLocalMonitor is enough — we
    // only want it when our window is key, not system-wide.
    private func registerFind() {
        NSEvent.addLocalMonitorForEvents(matching: .keyDown) { [weak self] e in
            guard let self,
                  e.modifierFlags.contains(.command),
                  e.charactersIgnoringModifiers == "f"
            else { return e }
            self.chat.searchToggle(); return nil
        }
    }

    private func registerHotkey() {
        var spec = EventTypeSpec(
            eventClass: OSType(kEventClassKeyboard),
            eventKind: UInt32(kEventHotKeyPressed))
        InstallEventHandler(GetEventDispatcherTarget(), { _, _, ud in
            let s = Unmanaged<AppController>.fromOpaque(ud!).takeUnretainedValue()
            DispatchQueue.main.async { s.chat.toggle() }
            return noErr
        }, 1, &spec, Unmanaged.passUnretained(self).toOpaque(), nil)
        let id = EventHotKeyID(signature: OSType(0x6e63_6862) /* 'nchb' */, id: 1)
        RegisterEventHotKey(UInt32(kVK_ANSI_G), UInt32(optionKey), id,
                            GetEventDispatcherTarget(), 0, &hotkey)
    }
}

// MARK: - main

func defaultSocket() -> String {
    if let x = ProcessInfo.processInfo.environment["XDG_RUNTIME_DIR"], !x.isEmpty {
        return x + "/nostr-chatd.sock"
    }
    // Match the daemon's Darwin fallback (os.TempDir = confstr).
    var t = NSTemporaryDirectory()
    if t.hasSuffix("/") { t.removeLast() }
    return t + "/nostr-chatd.sock"
}

var socket = defaultSocket()
var maxHistory = 200
do {
    var it = CommandLine.arguments.dropFirst().makeIterator()
    while let a = it.next() {
        switch a {
        case "--socket": if let v = it.next() { socket = v }
        case "--max-history": if let v = it.next(), let n = Int(v) { maxHistory = n }
        case "--help", "-h":
            print("usage: nostr-chat-bar [--socket PATH] [--max-history N]")
            exit(0)
        default:
            FileHandle.standardError.write(Data("unknown option: \(a)\n".utf8)); exit(1)
        }
    }
}

// Single-instance guard: a second copy (dev build vs launchd agent)
// would register ⌥G twice and show two 💬 items. flock the socket's
// sibling so the lock scopes to the same daemon we'd be talking to;
// O_EXLOCK|O_NONBLOCK fails fast if held. fd is leaked on purpose —
// the kernel drops the lock when we exit.
do {
    let lock = socket + ".bar.lock"
    let fd = open(lock, O_CREAT | O_RDWR | O_EXLOCK | O_NONBLOCK, 0o600)
    if fd < 0 {
        FileHandle.standardError.write(
            Data("nostr-chat-bar: already running (lock \(lock)): \(String(cString: strerror(errno)))\n".utf8))
        exit(0)
    }
}

extension Array {
    subscript(safe i: Int) -> Element? { indices.contains(i) ? self[i] : nil }
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let ctrl = AppController(socket: socket, maxHistory: maxHistory)
app.delegate = ctrl
app.run()
