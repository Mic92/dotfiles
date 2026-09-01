/**
 * `python` tool: a persistent interpreter (driver.py) per session, so the
 * model can keep parsed data, imports and figures between calls instead of
 * re-running scripts through bash. Uses `pi-python` (nix, with matplotlib/
 * polars/pyelftools/requests) unless $PI_PYTHON points elsewhere.
 */

import { execFileSync } from "node:child_process";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
  DEFAULT_MAX_BYTES,
  DEFAULT_MAX_LINES,
  formatSize,
  highlightCode,
  keyHint,
  truncateTail,
} from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "typebox";
import { Kernel } from "./kernel.ts";

function findPython(): string | undefined {
  for (const cand of [process.env.PI_PYTHON, "pi-python", "python3"]) {
    if (!cand) continue;
    try {
      execFileSync(cand, ["-c", ""], { stdio: "ignore" });
      return cand;
    } catch {}
  }
  return undefined;
}

export default function (pi: ExtensionAPI) {
  const python = findPython();
  if (!python) return;
  let kernel: Kernel | undefined;

  pi.on("session_shutdown", async () => {
    kernel?.stop();
    kernel = undefined;
  });

  pi.registerCommand("python-restart", {
    description: "Restart the persistent Python interpreter (drops all state)",
    handler: async (_args, ctx) => {
      kernel?.stop();
      kernel = undefined;
      ctx.ui.notify("Python interpreter reset.", "info");
    },
  });

  pi.registerTool({
    name: "python",
    label: "python",
    description:
      "Execute Python code in a persistent interpreter: variables, imports and open files survive between calls for the whole session. " +
      "The value of a trailing expression is echoed like in a REPL; use print() for anything else. " +
      "matplotlib figures are returned as images. Available: polars, matplotlib, requests, pyelftools + stdlib. " +
      `Output is truncated to the last ${DEFAULT_MAX_LINES} lines / ${
        formatSize(DEFAULT_MAX_BYTES)
      }.`,
    promptSnippet:
      "Run Python in a persistent interpreter (state kept across calls; polars, matplotlib, requests, pyelftools)",
    promptGuidelines: [
      "Use python instead of bash for multi-step data work (JSON/CSV/logs/ELF), calculations, and anything where re-parsing input on every call would be wasteful; state persists, so load once and iterate.",
    ],
    parameters: Type.Object({
      code: Type.String({ description: "Python source to execute" }),
      timeout: Type.Optional(
        Type.Number({
          description: "Timeout in seconds (interrupts the cell, keeps state)",
        }),
      ),
    }),

    // Without this pi's fallback renders only the tool name, hiding the code.
    renderCall(args, theme, context) {
      const text = (context.lastComponent as Text | undefined) ??
        new Text("", 0, 0);
      let out = theme.fg("toolTitle", theme.bold("python"));
      if (args.timeout) out += theme.fg("muted", ` (timeout ${args.timeout}s)`);
      const code = (args.code ?? "").trimEnd();
      if (code) out += "\n" + highlightCode(code, "python").join("\n");
      text.setText(out);
      return text;
    },

    renderResult(result, options, theme, context) {
      const text = (context.lastComponent as Text | undefined) ??
        new Text("", 0, 0);
      const output = result.content
        .filter((c) => c.type === "text")
        .map((c) => (c as { text: string }).text)
        .join("\n")
        .trimEnd();
      const images = result.content.filter((c) => c.type === "image").length;
      const lines = output ? output.split("\n") : [];
      // Tail, not head: tracebacks and REPL results come last.
      const shown = options.expanded ? lines : lines.slice(-10);
      const color = context.isError ? "error" : "toolOutput";
      let out = theme.fg("muted", "─── output ───");
      if (lines.length > shown.length) {
        out += theme.fg(
          "muted",
          `\n... (${lines.length - shown.length} earlier lines, `,
        ) + keyHint("app.tools.expand", "to expand") + theme.fg("muted", ")");
      }
      if (shown.length) {
        out += "\n" + shown.map((l) => theme.fg(color, l)).join("\n");
      }
      if (images) out += theme.fg("muted", `\n[${images} image(s)]`);
      text.setText(out);
      return text;
    },

    async execute(_id, params, signal, _onUpdate, ctx) {
      kernel ??= new Kernel(python, ctx.cwd);
      const ac = new AbortController();
      signal?.addEventListener("abort", () => ac.abort(), { once: true });
      const timer = params.timeout
        ? setTimeout(() => ac.abort(), params.timeout * 1000)
        : undefined;
      const r = await kernel.exec(params.code, ac.signal).finally(() =>
        clearTimeout(timer)
      );

      let text = r.stdout;
      if (r.stderr) {
        text += (text && !text.endsWith("\n") ? "\n" : "") + r.stderr;
      }
      if (r.result !== null) {
        text += (text && !text.endsWith("\n") ? "\n" : "") + r.result;
      }
      if (r.error) text += (text && !text.endsWith("\n") ? "\n" : "") + r.error;
      const t = truncateTail(text || (r.images.length ? "" : "(no output)"));
      if (t.truncated) {
        t.content =
          `[output truncated: showing last ${t.outputLines} of ${t.totalLines} lines]\n${t.content}`;
      }

      const content: ({ type: "text"; text: string } | {
        type: "image";
        data: string;
        mimeType: string;
      })[] = [];
      if (t.content) content.push({ type: "text", text: t.content });
      for (const data of r.images) {
        content.push({ type: "image", data, mimeType: "image/png" });
      }
      return { content, details: undefined, isError: r.error !== null };
    },
  });
}
