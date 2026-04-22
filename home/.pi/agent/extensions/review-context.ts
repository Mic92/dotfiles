/**
 * /review command — self-review the current branch's diff before merge.
 *
 * Injects base branch and diff stat so the agent has full context without
 * wasting a tool-call turn.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { git, localBase } from "./_git.ts";

const REVIEW_PROMPT = `
Self-review.

1. Read \`git diff <base>..HEAD\` line by line for bugs you introduced
2. Trim verbose/stale comments and dead code.
3. Run linter and tests.
4. Fix and amend. If clean, say so.
`.trim();

export default function (pi: ExtensionAPI) {
  pi.registerCommand("review", {
    description: "Self-review diff: hunt bugs, trim comments, lint/test, amend",
    handler: async (args, ctx) => {
      const base = args.trim() || (await localBase(pi.exec));
      const diffStat =
        (await git(pi.exec, ["diff", "--stat", `${base}..HEAD`], 10000)) ||
        "(no diff vs base)";

      const gitContext = [
        "## Review context",
        "",
        `Base branch: \`${base}\``,
        "",
        `### git diff --stat ${base}..HEAD`,
        "```",
        diffStat,
        "```",
      ].join("\n");

      pi.sendUserMessage(`${REVIEW_PROMPT}\n\n${gitContext}`, {
        deliverAs: "followUp",
      });
    },
  });
}
