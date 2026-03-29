/**
 * /cron — schedule recurring prompts that fire on an interval.
 *
 * Tasks only fire when the agent is idle (between turns). Each task gets
 * deterministic jitter derived from its ID so multiple tasks don't
 * thunder-herd, and so restarts don't reshuffle fire times.
 *
 * Usage:
 *   /cron 5m check if the deployment finished
 *   /cron 2h run the integration tests
 *   /cron check deploy every 30m
 *   /cron list          — show all tasks
 *   /cron delete <id>   — cancel a task
 *   /cron clear         — delete all tasks
 *
 * Based on aldoborrero/agent-kit (extensions/loop). Rewritten to drop the
 * croner dependency — upstream only accepts simple intervals (5m/2h) as
 * input anyway, so the cron-expression round-trip was just overhead.
 */
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";

interface CronTask {
  id: string;
  prompt: string;
  intervalMs: number;
  humanLabel: string;
  createdAt: number;
  expiresAt: number;
  fireCount: number;
  nextFireAt: number;
  jitterMs: number;
}

const MAX_TASKS = 50;
const EXPIRY_DAYS = 7;
const CHECK_INTERVAL_MS = 1000;
const MIN_INTERVAL_MS = 60_000; // 1m floor — anything faster is spam

function generateId(): string {
  return Math.random().toString(36).slice(2, 10);
}

/**
 * Deterministic jitter from task ID: up to 10% of the interval, cap 15min.
 * Hashing the ID means the same task gets the same jitter across restarts.
 */
function computeJitterMs(id: string, intervalMs: number): number {
  let h = 0;
  for (let i = 0; i < id.length; i++) {
    h = ((h << 5) - h + id.charCodeAt(i)) | 0;
  }
  const seed = Math.abs(h) / 2147483647;
  return Math.floor(seed * Math.min(intervalMs * 0.1, 15 * 60_000));
}

interface Parsed {
  intervalMs: number;
  humanLabel: string;
  prompt: string;
  clamped?: string;
}

const UNIT_MS: Record<string, number> = {
  s: 1_000,
  m: 60_000,
  h: 3_600_000,
  d: 86_400_000,
};

function normalizeUnit(u: string): string {
  const x = u.toLowerCase();
  if (x.startsWith("s")) return "s";
  if (x.startsWith("m")) return "m";
  if (x.startsWith("h")) return "h";
  if (x.startsWith("d")) return "d";
  return "m";
}

function labelFor(ms: number): string {
  if (ms % 86_400_000 === 0) return `every ${ms / 86_400_000}d`;
  if (ms % 3_600_000 === 0) return `every ${ms / 3_600_000}h`;
  if (ms % 60_000 === 0) return `every ${ms / 60_000}m`;
  return `every ${Math.round(ms / 1000)}s`;
}

/**
 * Parse "/cron <input>" into interval + prompt.
 *  1. Leading token: "5m check something"
 *  2. Trailing clause: "check something every 5m"
 *  3. Default: 10m, entire input is the prompt
 */
function parseInput(input: string): Parsed | null {
  const s = input.trim();
  if (!s) return null;

  let n: number | undefined;
  let unit: string | undefined;
  let prompt: string | undefined;

  const lead = s.match(/^(\d+)\s*(s|m|h|d)\s+(.+)$/i);
  if (lead?.[3].trim()) {
    n = parseInt(lead[1], 10);
    unit = lead[2];
    prompt = lead[3].trim();
  } else {
    const trail = s.match(
      /^(.+?)\s+every\s+(\d+)\s*(s|sec|seconds?|m|min|minutes?|h|hr|hours?|d|days?)\s*$/i,
    );
    if (trail?.[1].trim()) {
      n = parseInt(trail[2], 10);
      unit = trail[3];
      prompt = trail[1].trim();
    }
  }

  if (n === undefined || !unit || !prompt) {
    return {
      intervalMs: 10 * 60_000,
      humanLabel: "every 10m",
      prompt: s,
    };
  }

  const raw = n * UNIT_MS[normalizeUnit(unit)];
  const intervalMs = Math.max(raw, MIN_INTERVAL_MS);
  return {
    intervalMs,
    humanLabel: labelFor(intervalMs),
    prompt,
    clamped: raw < MIN_INTERVAL_MS
      ? `Clamped ${n}${normalizeUnit(unit)} up to 1m (minimum interval)`
      : undefined,
  };
}

function formatRelative(ms: number): string {
  if (ms <= 0) return "now";
  if (ms < 60_000) return `${Math.round(ms / 1000)}s`;
  if (ms < 3_600_000) return `${Math.round(ms / 60_000)}m`;
  if (ms < 86_400_000) return `${Math.round(ms / 3_600_000)}h`;
  return `${Math.round(ms / 86_400_000)}d`;
}

export default function (pi: ExtensionAPI) {
  const tasks = new Map<string, CronTask>();
  let timer: ReturnType<typeof setInterval> | null = null;
  let busy = false;
  let ctxRef: ExtensionContext | null = null;

  function updateStatus() {
    if (!ctxRef?.hasUI) return;
    ctxRef.ui.setStatus(
      "cron",
      tasks.size === 0
        ? undefined
        : ctxRef.ui.theme.fg("success", `cron:${tasks.size}`),
    );
  }

  function fireDue() {
    if (busy || !ctxRef) return;
    const now = Date.now();
    const drop: string[] = [];

    for (const [id, t] of tasks) {
      if (now >= t.expiresAt) {
        drop.push(id);
        if (ctxRef.hasUI) {
          ctxRef.ui.notify(
            `cron task ${id} expired (${EXPIRY_DAYS}d limit)`,
            "info",
          );
        }
        continue;
      }
      if (now >= t.nextFireAt) {
        t.fireCount++;
        t.nextFireAt = now + t.intervalMs + t.jitterMs;

        pi.sendMessage(
          {
            customType: "cron",
            content: `[cron ${t.id} — ${t.humanLabel}]\n\n${t.prompt}`,
            display: true,
          },
          { triggerTurn: true },
        );
        updateStatus();
        return; // one at a time — next due fires after this agent_end
      }
    }
    for (const id of drop) tasks.delete(id);
    if (drop.length) updateStatus();
  }

  const start = () => {
    if (!timer) timer = setInterval(fireDue, CHECK_INTERVAL_MS);
  };
  const stop = () => {
    if (timer) {
      clearInterval(timer);
      timer = null;
    }
  };

  pi.on("agent_start", async () => {
    busy = true;
  });
  pi.on("agent_end", async (_ev, ctx) => {
    busy = false;
    ctxRef = ctx;
    fireDue();
  });
  pi.on("session_start", async (_ev, ctx) => {
    ctxRef = ctx;
    start();
  });
  pi.on("session_shutdown", async () => {
    stop();
    tasks.clear();
  });

  pi.registerCommand("cron", {
    description:
      "Schedule recurring prompts (/cron 5m <prompt> | list | delete <id> | clear)",
    handler: async (args, ctx) => {
      ctxRef = ctx;
      const s = args.trim();

      if (!s) {
        if (tasks.size === 0) {
          ctx.ui.notify(
            "No scheduled tasks.\n\n" +
              "  /cron 5m check deploy   — schedule\n" +
              "  /cron list              — show all\n" +
              "  /cron delete <id>       — cancel",
            "info",
          );
        } else showList(ctx);
        return;
      }

      if (s === "list" || s === "ls") {
        showList(ctx);
        return;
      }

      const del = s.match(/^(?:delete|rm|stop)\s+(\S+)/);
      if (del) {
        if (tasks.delete(del[1])) {
          updateStatus();
          ctx.ui.notify(`Task ${del[1]} deleted`, "info");
          if (tasks.size === 0) stop();
        } else {
          ctx.ui.notify(`Task ${del[1]} not found`, "error");
        }
        return;
      }

      if (s === "clear" || s === "clear all") {
        const n = tasks.size;
        tasks.clear();
        stop();
        updateStatus();
        ctx.ui.notify(`Deleted ${n} task(s)`, "info");
        return;
      }

      if (tasks.size >= MAX_TASKS) {
        ctx.ui.notify(`Max ${MAX_TASKS} tasks. /cron clear first.`, "error");
        return;
      }

      const p = parseInput(s);
      if (!p?.prompt) {
        ctx.ui.notify("Usage: /cron [interval] <prompt>", "error");
        return;
      }

      const id = generateId();
      const now = Date.now();
      const jitter = computeJitterMs(id, p.intervalMs);
      const t: CronTask = {
        id,
        prompt: p.prompt,
        intervalMs: p.intervalMs,
        humanLabel: p.humanLabel,
        createdAt: now,
        expiresAt: now + EXPIRY_DAYS * 86_400_000,
        fireCount: 0,
        nextFireAt: now + p.intervalMs + jitter,
        jitterMs: jitter,
      };
      tasks.set(id, t);
      start();
      updateStatus();

      let msg = `Scheduled ${id} (${p.humanLabel})\n` +
        `Next: in ${formatRelative(t.nextFireAt - now)}\n` +
        `Expires in ${EXPIRY_DAYS}d. /cron delete ${id} to cancel.`;
      if (p.clamped) msg = `${p.clamped}\n\n${msg}`;
      ctx.ui.notify(msg, "info");

      // Fire once immediately so you see it working.
      pi.sendMessage(
        {
          customType: "cron",
          content:
            `[cron ${id} — ${p.humanLabel} — initial run]\n\n${p.prompt}`,
          display: true,
        },
        { triggerTurn: true },
      );
    },
  });

  function showList(ctx: ExtensionContext) {
    if (tasks.size === 0) {
      ctx.ui.notify("No scheduled tasks", "info");
      return;
    }
    const now = Date.now();
    const lines = Array.from(tasks.values()).map(
      (t) =>
        `${t.id}  ${t.humanLabel.padEnd(10)} fired ${
          String(t.fireCount).padStart(3)
        }x  next in ${formatRelative(t.nextFireAt - now)}\n  ${t.prompt}`,
    );
    ctx.ui.notify(
      `Scheduled tasks (${tasks.size}):\n\n${lines.join("\n\n")}`,
      "info",
    );
  }
}
