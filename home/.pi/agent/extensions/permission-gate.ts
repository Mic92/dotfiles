/**
 * Permission Gate Extension
 *
 * Prompts for confirmation before running potentially dangerous bash commands.
 * Based on upstream example: examples/extensions/permission-gate.ts
 *
 * Toggle with /permission-gate command.
 * Status bar shows "gate ■" when active.
 */

import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
  Editor,
  type EditorTheme,
  Key,
  matchesKey,
  truncateToWidth,
} from "@mariozechner/pi-tui";

type GateResult = { allow: true } | { allow: false; reason: string };

interface PatternEntry {
  pattern: RegExp;
  label: string;
}

interface PatternJSON {
  pattern: string;
  label: string;
  flags?: string;
}

/**
 * Generic defaults shipped with the extension. Users can override by
 * creating ~/.pi/agent/permission-gate.json or .pi/permission-gate.json
 * (project-local). See loadPatterns() for resolution order.
 */
const DEFAULT_PATTERNS: PatternJSON[] = [
  { pattern: "\\brm\\s+(-[^\\s]*r|--recursive)", label: "recursive delete" },
  { pattern: "\\bsudo\\b", label: "sudo" },
  { pattern: "\\bchmod\\b.*777", label: "world-writable permissions" },
  { pattern: ">\\s*/dev/[sh]d[a-z]", label: "raw device redirect" },
  { pattern: "\\bgit\\s+push\\s+.*(-f\\b|--force\\b)", label: "force push" },
  { pattern: "\\bgit\\s+reset\\s+--hard\\b", label: "hard reset" },
  { pattern: "\\bgit\\s+clean\\s+-[^\\s]*f", label: "git clean" },
  { pattern: "\\bgit\\s+restore\\b", label: "git restore" },
  { pattern: "\\b(curl|wget)\\b.*\\|\\s*(ba)?sh\\b", label: "pipe to shell" },
];

function compile(entries: PatternJSON[]): PatternEntry[] {
  return entries.map((e) => ({
    pattern: new RegExp(e.pattern, e.flags ?? "i"),
    label: e.label,
  }));
}

/**
 * Resolution order (last wins / overrides):
 *   1. built-in DEFAULT_PATTERNS
 *   2. ~/.pi/agent/permission-gate.json
 *   3. <cwd>/.pi/permission-gate.json
 *
 * File format: { "replace"?: bool, "patterns": [{pattern, label, flags?}, ...] }
 * If "replace" is true the file replaces everything loaded so far,
 * otherwise it appends.
 */
function loadPatterns(cwd: string, warn: (msg: string) => void): PatternEntry[] {
  let raw: PatternJSON[] = [...DEFAULT_PATTERNS];

  const candidates = [
    join(homedir(), ".pi", "agent", "permission-gate.json"),
    join(cwd, ".pi", "permission-gate.json"),
  ];

  for (const file of candidates) {
    if (!existsSync(file)) continue;
    try {
      const parsed = JSON.parse(readFileSync(file, "utf8")) as {
        replace?: boolean;
        patterns?: PatternJSON[];
      };
      if (!Array.isArray(parsed.patterns)) {
        warn(`permission-gate: ${file}: missing "patterns" array, ignoring`);
        continue;
      }
      raw = parsed.replace ? parsed.patterns : [...raw, ...parsed.patterns];
    } catch (err) {
      warn(`permission-gate: failed to load ${file}: ${(err as Error).message}`);
    }
  }

  try {
    return compile(raw);
  } catch (err) {
    warn(`permission-gate: invalid regex, falling back to defaults: ${(err as Error).message}`);
    return compile(DEFAULT_PATTERNS);
  }
}

export default function (pi: ExtensionAPI) {
  let enabled = true;
  let dangerousPatterns: PatternEntry[] = compile(DEFAULT_PATTERNS);

  pi.registerCommand("permission-gate", {
    description:
      "Toggle permission gate — confirm dangerous commands before running",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) return;

      enabled = !enabled;
      if (enabled) {
        ctx.ui.setStatus(
          "permission-gate",
          ctx.ui.theme.fg("warning", "gate ■"),
        );
        ctx.ui.notify(
          "Permission gate enabled — dangerous commands require approval",
          "info",
        );
      } else {
        ctx.ui.setStatus("permission-gate", undefined);
        ctx.ui.notify("Permission gate disabled", "info");
      }
    },
  });

  // Show status on startup and (re)load patterns from disk.
  pi.on("session_start", async (_event, ctx) => {
    dangerousPatterns = loadPatterns(ctx.cwd, (msg) =>
      ctx.hasUI ? ctx.ui.notify(msg, "warning") : console.error(msg),
    );
    if (enabled && ctx.hasUI) {
      ctx.ui.setStatus("permission-gate", ctx.ui.theme.fg("warning", "gate ■"));
    }
  });

  pi.on("tool_call", async (event, ctx) => {
    if (!enabled) return undefined;
    if (event.toolName !== "bash") return undefined;

    const command = event.input.command as string;
    const matched = dangerousPatterns.filter((p) => p.pattern.test(command));

    if (matched.length > 0) {
      const labels = matched.map((m) => m.label).join(", ");

      if (!ctx.hasUI) {
        return {
          block: true,
          reason:
            `Dangerous command blocked (${labels}) — no UI for confirmation`,
        };
      }

      pi.events.emit("permission-gate:waiting");

      const result = await ctx.ui.custom<GateResult>(
        (tui, theme, _kb, done) => {
          let optionIndex = 0; // 0 = Yes, 1 = No
          let inputMode = false;
          let cachedLines: string[] | undefined;

          const editorTheme: EditorTheme = {
            borderColor: (s) => theme.fg("accent", s),
            selectList: {
              selectedPrefix: (t) => theme.fg("accent", t),
              selectedText: (t) => theme.fg("accent", t),
              description: (t) => theme.fg("muted", t),
              scrollInfo: (t) => theme.fg("dim", t),
              noMatch: (t) => theme.fg("warning", t),
            },
          };
          const editor = new Editor(tui, editorTheme);

          function refresh() {
            cachedLines = undefined;
            tui.requestRender();
          }

          editor.onSubmit = (value) => {
            const reason = value.trim()
              ? `Blocked by user (${labels}): ${value.trim()}`
              : `Blocked by user (${labels})`;
            done({ allow: false, reason });
          };

          function handleInput(data: string) {
            if (inputMode) {
              if (matchesKey(data, Key.escape)) {
                inputMode = false;
                editor.setText("");
                refresh();
                return;
              }
              editor.handleInput(data);
              refresh();
              return;
            }

            if (matchesKey(data, Key.up)) {
              optionIndex = 0;
              refresh();
              return;
            }
            if (matchesKey(data, Key.down)) {
              optionIndex = 1;
              refresh();
              return;
            }
            if (matchesKey(data, Key.enter)) {
              if (optionIndex === 0) {
                done({ allow: true });
              } else {
                inputMode = true;
                editor.setText("");
                refresh();
              }
              return;
            }
            if (matchesKey(data, Key.escape)) {
              done({ allow: false, reason: `Blocked by user (${labels})` });
            }
          }

          function render(width: number): string[] {
            if (cachedLines) return cachedLines;
            const lines: string[] = [];
            const add = (s: string) => lines.push(truncateToWidth(s, width));

            lines.push("");
            add(
              theme.fg("warning", " ⚠️  Dangerous command ") +
                theme.fg("muted", `(${labels})`),
            );
            add(`    ${theme.fg("text", command)}`);
            lines.push("");

            const opts = ["Yes", inputMode ? "No ✎" : "No"];
            for (let i = 0; i < opts.length; i++) {
              const selected = i === optionIndex;
              const prefix = selected ? theme.fg("accent", "> ") : "  ";
              add(` ${prefix}${theme.fg(selected ? "accent" : "text", opts[i])}`);
            }
            lines.push("");

            if (inputMode) {
              add(theme.fg("muted", " Reason:"));
              for (const line of editor.render(width - 2)) {
                add(` ${line}`);
              }
              lines.push("");
              add(theme.fg("dim", " Enter submit • Esc back"));
            } else {
              add(theme.fg("dim", " ↑↓ • Enter • Esc block"));
            }
            lines.push("");

            cachedLines = lines;
            return lines;
          }

          return {
            render,
            invalidate: () => {
              cachedLines = undefined;
            },
            handleInput,
          };
        },
      );

      pi.events.emit("permission-gate:resolved");

      if (!result.allow) {
        return { block: true, reason: result.reason };
      }
    }

    return undefined;
  });
}
