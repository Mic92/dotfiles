/**
 * /commit command — commits staged changes (or all if nothing staged).
 *
 * Injects live `git status` and `git log` output so the agent has full
 * context without wasting a tool-call turn.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

const COMMIT_PROMPT = `
Commit the changes using this style:

- when writing commit messages focus on the WHY rather than the WHAT.
- imperative mood
- concise, no conventional commit prefixes
- optionally use a context prefix when it adds clarity (e.g., "docs:", "cli:")
- paragraphs instead of bullet points

If nothing is staged, stage all changes first (\`git add -A\`).
If there are untracked files relevant to the change, include them.

If the changes cover multiple unrelated concerns, split them into separate
logical commits rather than one large commit.
`.trim();

export default function (pi: ExtensionAPI) {
  pi.registerCommand("commit", {
    description: "Commit staged changes (or all if nothing staged)",
    handler: async (_args, ctx) => {
      const [status, log] = await Promise.all([
        pi.exec("git", ["status"], { timeout: 5000 }),
        pi.exec("git", ["log", "--oneline", "-5"], { timeout: 5000 }),
      ]);

      const gitContext = [
        "## Current git state",
        "",
        "### git status",
        "```",
        status.stdout.trim(),
        "```",
        "",
        "### git log --oneline -5",
        "```",
        log.stdout.trim(),
        "```",
      ].join("\n");

      pi.sendUserMessage(`${COMMIT_PROMPT}\n\n${gitContext}`, {
        deliverAs: "followUp",
      });
    },
  });
}
