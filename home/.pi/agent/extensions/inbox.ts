/**
 * inbox — per-session unix socket for async events into the conversation.
 *
 * Tool subprocesses get $PI_INBOX (socket path) and $PI_SESSION_ID. Each
 * connection to the socket becomes one message (JSON `{source?, text}` or
 * plain text), delivered immediately if idle, otherwise as a follow-up.
 * Used by `queue` for detached tasks and `notify()` in the python kernel.
 *
 *   echo '{"source":"ci","text":"build green"}' | nc -U "$PI_INBOX"
 */

import { mkdirSync, rmSync } from "node:fs";
import { createServer } from "node:net";
import { basename, join } from "node:path";
import { text } from "node:stream/consumers";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";

const MAX_BYTES = 16 * 1024;
const TAIL_LINES = 12;

type Event = { source?: string; text: string };

function parse(raw: string): Event {
  try {
    const j = JSON.parse(raw);
    if (typeof j?.text === "string") return { source: j.source, text: j.text };
  } catch {}
  return { text: raw.trim() };
}

export default function (pi: ExtensionAPI) {
  const path = join(process.env.XDG_RUNTIME_DIR ?? "/tmp", "pi-inbox", `${process.pid}.sock`);

  pi.registerMessageRenderer("inbox", (message, _options, theme) => {
    const ev = message.details as Event;
    let lines = ev.text.split("\n");
    if (lines.length > TAIL_LINES) {
      lines = [`… ${lines.length - TAIL_LINES} lines`, ...lines.slice(-TAIL_LINES)];
    }
    const title = theme.fg("accent", theme.bold(`▌inbox ${ev.source ?? ""}`.trimEnd()));
    return new Text(`${title}\n${theme.fg("muted", lines.join("\n"))}`, 0, 0);
  });

  // At load, not session_start, so other extensions see $PI_INBOX early.
  mkdirSync(join(path, ".."), { recursive: true, mode: 0o700 });
  rmSync(path, { force: true });
  const server = createServer(async (conn) => {
      const raw = (await text(conn).catch(() => "")).slice(0, MAX_BYTES);
      const ev = parse(raw);
      if (!ev.text) return;
      const header = ev.source ? `[inbox: ${ev.source}]` : "[inbox]";
      pi.sendMessage(
        { customType: "inbox", content: `${header}\n${ev.text}`, display: true, details: ev },
        { deliverAs: "followUp", triggerTurn: true },
      );
  }).listen(path);
  process.env.PI_INBOX = path;

  pi.on("session_start", async (_event, ctx) => {
    const file = ctx.sessionManager.getSessionFile();
    process.env.PI_SESSION_ID = file ? basename(file, ".jsonl") : `pid-${process.pid}`;
  });

  pi.on("session_shutdown", async () => {
    server.close();
    rmSync(path, { force: true });
    delete process.env.PI_INBOX;
  });
}
