/**
 * RTK (Rust Token Killer) Extension for pi
 *
 * Transparently rewrites bash commands to their rtk equivalents via tool_call
 * mutation, saving 60-90% of tokens on common operations.
 *
 * Discovers available rtk commands at startup by parsing `rtk --help` and
 * `rtk <cmd> help`, so new rtk features are picked up automatically.
 *
 * Only rewrites commands where rtk accepts passthrough [ARGS]... or has known
 * subcommands. Commands where rtk has its own positional interface (grep, find,
 * read, diff) are left alone to avoid breaking flags.
 *
 * Requires: rtk in PATH (https://github.com/rtk-ai/rtk)
 */

import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import { isToolCallEventType } from "@mariozechner/pi-coding-agent";

// rtk meta-commands that aren't rewrite targets.
const IGNORED_COMMANDS = new Set([
  "init",
  "gain",
  "cc-economics",
  "config",
  "discover",
  "learn",
  "proxy",
  "hook-audit",
  "help",
  "smart",
  "summary",
  "err",
  "test",
  "json",
  "deps",
  "env",
  "log",
  "wc",
]);

// Flags that can appear before a subcommand in git/docker/kubectl.
const PRE_SUBCMD_FLAGS = [
  /(-C|-c)\s+\S+\s*/g,
  /--[a-z-]+=\S+\s*/g,
  /--(no-pager|no-optional-locks|bare|literal-pathspecs)\s*/g,
  /(-H|--context|--config|--kubeconfig|--namespace|-n)\s+\S+\s*/g,
];

interface RewriteConfig {
  simple: Set<string>; // passthrough [ARGS]...
  withSubs: Map<string, Set<string>>; // cmd → known subcommands
  ownInterface: Set<string>; // own positional args, skip
  composeSubs: Set<string> | null; // docker compose subcommands
}

// --- Discovery ---

async function parseCmdInfo(pi: ExtensionAPI, cmd: string) {
  try {
    const { stdout } = await pi.exec("rtk", [cmd, "help"], { timeout: 5000 });
    const lines = (stdout || "").split("\n");
    let inCommands = false;
    const subs = new Set<string>();
    let passthrough = false;

    for (const line of lines) {
      if (/^Usage:/.test(line) && /\[ARGS\]\.\.\./.test(line)) {
        passthrough = true;
      }
      if (/^Commands:/.test(line)) {
        inCommands = true;
        continue;
      }
      if (inCommands) {
        if (/^\s*$/.test(line) || /^Options:/.test(line)) break;
        const m = line.match(/^\s+(\S+)/);
        if (m && m[1] !== "help") subs.add(m[1]);
      }
    }
    return { subs: subs.size > 0 ? subs : null, passthrough };
  } catch {
    return { subs: null, passthrough: false };
  }
}

async function discoverConfig(pi: ExtensionAPI): Promise<RewriteConfig> {
  const config: RewriteConfig = {
    simple: new Set(),
    withSubs: new Map(),
    ownInterface: new Set(),
    composeSubs: null,
  };
  try {
    const { stdout } = await pi.exec("rtk", ["--help"], { timeout: 5000 });
    const lines = (stdout || "").split("\n");
    let inCommands = false;
    const topLevel: string[] = [];

    for (const line of lines) {
      if (/^Commands:/.test(line)) {
        inCommands = true;
        continue;
      }
      if (inCommands) {
        if (/^\s*$/.test(line) || /^Options:/.test(line)) break;
        const m = line.match(/^\s+(\S+)/);
        if (m && !IGNORED_COMMANDS.has(m[1])) topLevel.push(m[1]);
      }
    }

    const checks = await Promise.all(
      topLevel.map(async (cmd) => ({ cmd, ...(await parseCmdInfo(pi, cmd)) })),
    );
    for (const { cmd, subs, passthrough } of checks) {
      if (subs) config.withSubs.set(cmd, subs);
      else if (passthrough) config.simple.add(cmd);
      else config.ownInterface.add(cmd);
    }
    // Discover docker compose subcommands if docker has compose
    if (config.withSubs.get("docker")?.has("compose")) {
      try {
        const { stdout } = await pi.exec("rtk", ["docker", "compose", "help"], {
          timeout: 5000,
        });
        const lines = (stdout || "").split("\n");
        let inCommands = false;
        const subs = new Set<string>();
        for (const line of lines) {
          if (/^Commands:/.test(line)) {
            inCommands = true;
            continue;
          }
          if (inCommands) {
            if (/^\s*$/.test(line) || /^Options:/.test(line)) break;
            const m = line.match(/^\s+(\S+)/);
            if (m && m[1] !== "help") subs.add(m[1]);
          }
        }
        if (subs.size > 0) config.composeSubs = subs;
      } catch { /* no compose subs → docker compose won't be rewritten */ }
    }
  } catch { /* empty config → nothing rewritten */ }
  return config;
}

// --- Rewriter ---

function makeRewriter(config: RewriteConfig) {
  const isRewritable = (c: string) =>
    config.simple.has(c) || config.withSubs.has(c);

  // Identity mappings for rewritable commands only (excludes own-interface).
  const sourceToRtk = new Map<string, string>();
  for (const cmd of config.simple) sourceToRtk.set(cmd, cmd);
  for (const cmd of config.withSubs.keys()) sourceToRtk.set(cmd, cmd);

  function stripPreSubcmdFlags(s: string): string {
    for (const re of PRE_SUBCMD_FLAGS) s = s.replace(re, "");
    return s.trim();
  }

  function escapeRegExp(s: string): string {
    return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  }

  return function rewrite(cmd: string): string | null {
    if (/^rtk\s/.test(cmd) || /\/rtk\s/.test(cmd)) return null;
    if (cmd.includes("<<")) return null;

    const envMatch = cmd.match(/^((?:[A-Za-z_][A-Za-z0-9_]*=[^\s]*\s+)+)/);
    const envPrefix = envMatch?.[1] ?? "";
    const body = envPrefix ? cmd.slice(envPrefix.length) : cmd;
    const prefix = (rewritten: string) => `${envPrefix}${rewritten}`;

    // --- Multi-word source commands ---

    // python -m <tool> → rtk <tool>
    const pyMod = body.match(/^python\s+-m\s+(\S+)/);
    if (pyMod && isRewritable(pyMod[1])) {
      return prefix(body.replace(`python -m ${pyMod[1]}`, `rtk ${pyMod[1]}`));
    }

    // uv pip → rtk pip
    if (/^uv\s+pip\s/.test(body) && isRewritable("pip")) {
      return prefix(body.replace(/^uv pip /, "rtk pip "));
    }

    // npx <tool> → rtk <tool> if known, else rtk npx <tool>
    if (/^npx\s+/.test(body)) {
      const target = body.replace(/^npx\s+/, "").split(/\s/)[0];
      if (isRewritable(target)) return prefix(body.replace(/^npx\s+/, "rtk "));
      if (isRewritable("npx")) return prefix(body.replace(/^npx /, "rtk npx "));
      return null;
    }

    // pnpm <sub>: test → vitest, lint/tsc → direct, list/outdated → rtk pnpm
    if (/^pnpm\s/.test(body)) {
      const sub = body.replace(/^pnpm\s+/, "").split(/\s/)[0];
      if (sub === "test" && isRewritable("vitest")) {
        return prefix(body.replace(/^pnpm test/, "rtk vitest run"));
      }
      if (isRewritable(sub)) {
        return prefix(body.replace(`pnpm ${sub}`, `rtk ${sub}`));
      }
      const pnpmSubs = config.withSubs.get("pnpm");
      if (pnpmSubs?.has(sub)) {
        return prefix(body.replace(/^pnpm /, "rtk pnpm "));
      }
      return null;
    }

    // npm test/run → rtk npm test/run
    if (/^npm\s/.test(body) && isRewritable("npm")) {
      const sub = body.replace(/^npm\s+/, "").split(/\s/)[0];
      if (sub === "test" || sub === "run") {
        return prefix(body.replace(/^npm /, "rtk npm "));
      }
      return null;
    }

    // --- Generic: first word lookup ---

    const firstWord = body.split(/\s/)[0];
    const rtkCmd = sourceToRtk.get(firstWord);
    if (!rtkCmd) return null;

    const subs = config.withSubs.get(rtkCmd);
    if (subs) {
      const rest = body.replace(
        new RegExp(`^${escapeRegExp(firstWord)}\\s+`),
        "",
      );
      const subCmd = stripPreSubcmdFlags(rest).split(/\s/)[0];

      // docker compose <sub> → only rewrite if <sub> is known to rtk docker compose
      if (
        firstWord === "docker" && subCmd === "compose" && subs.has("compose")
      ) {
        const composeRest = rest.replace(/^compose\s*/, "").trim();
        const composeSub = stripPreSubcmdFlags(composeRest).split(/\s/)[0];
        const composeSubs = config.composeSubs;
        if (composeSubs && composeSubs.has(composeSub)) {
          return prefix(body.replace(/^docker /, "rtk docker "));
        }
        return null;
      }
      if (!subs.has(subCmd)) return null;
    }

    if (firstWord === rtkCmd) return prefix(`rtk ${body}`);
    return prefix(
      body.replace(
        new RegExp(`^${escapeRegExp(firstWord)} `),
        `rtk ${rtkCmd} `,
      ),
    );
  };
}

// --- Extension entry point ---

export default function (pi: ExtensionAPI) {
  let enabled = true;
  let rtkAvailable = false;
  let rewrite: ReturnType<typeof makeRewriter> | null = null;

  function restoreState(ctx: ExtensionContext) {
    for (const entry of ctx.sessionManager.getBranch()) {
      if (entry.type === "custom" && entry.customType === "rtk-state") {
        const data = entry.data as { enabled?: boolean } | undefined;
        if (data?.enabled !== undefined) enabled = data.enabled;
      }
    }
  }

  function updateStatus(ctx: ExtensionContext) {
    if (rtkAvailable) ctx.ui.setStatus("rtk", enabled ? "rtk ✓" : "rtk ⏸");
  }

  pi.on("session_start", async (_event, ctx) => {
    try {
      rtkAvailable =
        (await pi.exec("rtk", ["--version"], { timeout: 5000 })).code === 0;
    } catch {
      rtkAvailable = false;
    }

    if (!rtkAvailable) {
      enabled = false;
      ctx.ui.setStatus("rtk", "rtk ✗ (not found)");
      return;
    }
    rewrite = makeRewriter(await discoverConfig(pi));
    restoreState(ctx);
    updateStatus(ctx);
  });

  for (const evt of ["session_tree", "session_fork"] as const) {
    pi.on(evt, async (_e, ctx) => {
      restoreState(ctx);
      updateStatus(ctx);
    });
  }

  // Rewrite bash commands. Runs after permission-gate sees the original
  // command — "rtk git push --force" still matches \bgit\s+push.*--force.
  pi.on("tool_call", async (event) => {
    if (!enabled || !rewrite) return;
    if (!isToolCallEventType("bash", event)) return;
    const { command } = event.input;
    if (!command) return;

    if (!/[|]/.test(command) && !/&&|;/.test(command)) {
      const r = rewrite(command.trim());
      if (r) event.input.command = r;
    } else {
      const m = command.match(/^(.*?)(\s*(?:\|\||&&|[|;])\s*)(.*)$/s);
      if (m) {
        const r = rewrite(m[1].trim());
        if (r) event.input.command = r + m[2] + m[3];
      }
    }
  });

  // --- Commands ---

  pi.registerCommand("rtk", {
    description:
      "Show RTK token savings (graph, history, daily, weekly, monthly, all, discover)",
    handler: async (args, ctx) => {
      if (!rtkAvailable) {
        ctx.ui.notify("rtk not installed", "error");
        return;
      }
      const sub = (args || "").trim();
      if (sub === "discover") {
        try {
          const r = await pi.exec("rtk", ["discover"], { timeout: 30000 });
          ctx.ui.notify((r.stdout || r.stderr || "No output").trim(), "info");
        } catch {
          ctx.ui.notify("Failed to run rtk discover", "error");
        }
        return;
      }
      const flagMap: Record<string, string> = {
        graph: "--graph",
        history: "--history",
        daily: "--daily",
        weekly: "--weekly",
        monthly: "--monthly",
        all: "--all",
      };
      const gainArgs = [
        "gain",
        ...(flagMap[sub] ? [flagMap[sub]] : sub ? sub.split(/\s+/) : []),
      ];
      try {
        const r = await pi.exec("rtk", gainArgs, { timeout: 10000 });
        ctx.ui.notify((r.stdout || r.stderr || "No data").trim(), "info");
      } catch {
        ctx.ui.notify("Failed to run rtk gain", "error");
      }
    },
  });

  pi.registerCommand("rtk-toggle", {
    description: "Toggle RTK command rewriting on/off",
    handler: async (_args, ctx) => {
      if (!rtkAvailable) {
        ctx.ui.notify("rtk not installed", "error");
        return;
      }
      enabled = !enabled;
      pi.appendEntry("rtk-state", { enabled });
      updateStatus(ctx);
      ctx.ui.notify(enabled ? "RTK enabled" : "RTK disabled", "info");
    },
  });
}
