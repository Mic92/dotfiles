/**
 * Notify — native terminal notification when the agent finishes.
 *
 * Useful when pi is in a background tmux pane and you've tabbed away.
 * Title shows the project name; body shows the first sentence of the
 * last assistant message plus run stats (elapsed · turns · files).
 *
 * Protocols:
 *   OSC 777 — Ghostty, iTerm2, WezTerm, foot, rxvt-unicode (default)
 *   OSC 99  — Kitty
 *
 * Toggle with /notify or start with --no-notify.
 *
 * Ported from aldoborrero/agent-kit (Windows toast dropped).
 */
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import { basename } from "node:path";

/** Strip control chars and semicolons that break OSC escape sequences. */
function sanitize(s: string): string {
  // eslint-disable-next-line no-control-regex
  return s.replace(/[\x00-\x1f\x7f;]/g, " ").trim();
}

function notify(title: string, body: string): void {
  const t = sanitize(title);
  const b = sanitize(body);
  if (process.env.KITTY_WINDOW_ID) {
    // Kitty OSC 99: i=id, d=0 means "more parts follow", p=body marks the body part
    process.stdout.write(`\x1b]99;i=1:d=0;${t}\x1b\\`);
    process.stdout.write(`\x1b]99;i=1:p=body;${b}\x1b\\`);
  } else {
    process.stdout.write(`\x1b]777;notify;${t};${b}\x07`);
  }
}

export default function (pi: ExtensionAPI) {
  let enabled = true;
  let startMs: number | null = null;
  let turns = 0;
  let filesChanged = 0;

  pi.registerFlag("notify", {
    description: "Desktop notification when the agent finishes (default: on)",
    type: "boolean",
    default: true,
  });

  function updateStatus(ctx: ExtensionContext): void {
    if (!ctx.hasUI) return;
    // Only show when off — on is the default, no need to clutter the bar.
    ctx.ui.setStatus(
      "notify",
      enabled ? undefined : ctx.ui.theme.fg("warning", "notify:off"),
    );
  }

  pi.on("session_start", async (_ev, ctx) => {
    enabled = pi.getFlag("notify") !== false;
    updateStatus(ctx);
  });

  pi.registerCommand("notify", {
    description: "Toggle desktop notifications on/off",
    handler: async (_args, ctx) => {
      enabled = !enabled;
      updateStatus(ctx);
      ctx.ui.notify(
        enabled ? "Notifications enabled" : "Notifications disabled",
        "info",
      );
    },
  });

  pi.on("agent_start", async () => {
    startMs = Date.now();
    turns = 0;
    filesChanged = 0;
  });

  pi.on("turn_start", async (ev) => {
    turns = ev.turnIndex + 1;
  });

  pi.on("tool_result", async (ev) => {
    if ((ev.toolName === "edit" || ev.toolName === "write") && !ev.isError) {
      filesChanged++;
    }
  });

  pi.on("agent_end", async (ev, ctx) => {
    if (!enabled) return;

    const project = basename(ctx.cwd) || ctx.cwd;
    const elapsed = startMs !== null
      ? Math.round((Date.now() - startMs) / 1000)
      : null;

    // First sentence of the last assistant text block, capped at 80 chars.
    let snippet = "";
    for (let i = ev.messages.length - 1; i >= 0; i--) {
      const m = ev.messages[i] as {
        role?: string;
        content?: Array<{ type?: string; text?: string }>;
      };
      if (m.role !== "assistant") continue;
      const text = (m.content ?? [])
        .filter((c) => c.type === "text")
        .map((c) => c.text ?? "")
        .join(" ")
        .trim();
      if (text) {
        snippet = text.split(/[.!?\n]/)[0].trim().slice(0, 80);
        break;
      }
    }

    const stats: string[] = [];
    if (elapsed !== null) stats.push(`${elapsed}s`);
    if (turns > 0) stats.push(`${turns} turn${turns === 1 ? "" : "s"}`);
    if (filesChanged > 0) {
      stats.push(`${filesChanged} file${filesChanged === 1 ? "" : "s"}`);
    }
    const statLine = stats.join(" · ");

    const body = snippet
      ? statLine ? `${snippet} (${statLine})` : snippet
      : statLine || "Ready for input";

    notify(`Pi · ${project}`, body);
  });
}
