/**
 * Inline Bash Extension - expands !{command} patterns in user prompts.
 *
 * Ported from aldoborrero/agent-kit
 *
 * Usage:
 *   What's in !{pwd}?
 *   Branch is !{git branch --show-current}, status: !{git status --short}
 *
 * The !{command} patterns are executed and replaced with their output before
 * the prompt is sent to the LLM.
 *
 * Regular !command syntax (whole-line bash) is preserved unchanged.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

const PATTERN = /!\{([^}]+)\}/g;
const TIMEOUT_MS = 30000;
const MAX_OUTPUT = 10 * 1024; // 10KB per expansion

export default function (pi: ExtensionAPI) {
  pi.on("input", async (event, ctx) => {
    const text = event.text;

    // Preserve existing whole-line !command behavior
    const trimmed = text.trimStart();
    if (trimmed.startsWith("!") && !trimmed.startsWith("!{")) {
      return { action: "continue" };
    }

    // Collect matches upfront (avoid mutation-while-iterating)
    const matches: Array<{ full: string; command: string }> = [];
    for (const m of text.matchAll(PATTERN)) {
      matches.push({ full: m[0], command: m[1] });
    }
    if (matches.length === 0) {
      return { action: "continue" };
    }

    let result = text;
    const expansions: Array<
      { command: string; output: string; error?: string }
    > = [];

    for (const { full, command } of matches) {
      try {
        const r = await pi.exec("bash", ["-c", command], {
          timeout: TIMEOUT_MS,
        });
        let output = (r.stdout || r.stderr || "").trim();

        if (output.length > MAX_OUTPUT) {
          output = output.slice(0, MAX_OUTPUT) + "\n[... truncated at 10KB]";
        }

        const error = r.code !== 0 && r.stderr
          ? `exit code ${r.code}`
          : undefined;
        expansions.push({ command, output, error });
        // Use replacer fn so $&, $1, $` etc. in command output don't get
        // interpreted as replacement patterns by String.replace.
        result = result.replace(full, () => output);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        expansions.push({ command, output: "", error: msg });
        result = result.replace(full, () => `[error: ${msg}]`);
      }
    }

    if (ctx.hasUI) {
      const summary = expansions
        .map((e) => {
          const status = e.error ? ` (${e.error})` : "";
          const preview = e.output.length > 50
            ? `${e.output.slice(0, 50)}...`
            : e.output;
          return `!{${e.command}}${status} -> "${preview}"`;
        })
        .join("\n");
      ctx.ui.notify(
        `Expanded ${expansions.length} inline command(s):\n${summary}`,
        "info",
      );
    }

    return { action: "transform", text: result, images: event.images };
  });
}
