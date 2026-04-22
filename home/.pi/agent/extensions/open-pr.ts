/**
 * /open-pr command — write a PR description and open it in the browser.
 *
 * Adapted from https://github.com/raine/workmux/blob/main/skills/open-pr/SKILL.md
 *
 * Preloads git diff, log, and status so the agent can write the description
 * without spending tool-call turns gathering context.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { currentBranch, type Exec, git, workmuxBase } from "./_git.ts";

const OPEN_PR_PROMPT = `
Write a PR description using the conversation context and open PR creation in
the browser.

## Commit uncommitted changes

If \`git status\` below shows changes, commit them before proceeding.

## Write PR description

If a repo PR template was found below, fill it in.

Otherwise write a single short paragraph. Derive it from the commit messages:
for a single commit, expand its body slightly; for multiple commits,
synthesize the overall goal (don't enumerate them). No headings, no bullet
lists. Only mention testing if the conversation or diff shows actual
verification.

Guidelines:

- Lead with a concise summary of what the PR does
- Explain the "why" before the "how"
- Use the conversation context to inform the description
- Include before/after comparisons for UI or performance changes
- Be direct and to the point

## Create the PR

1. Write a short PR title (max 72 characters)
2. Ensure the branch is pushed: \`git push -u origin HEAD\`
3. Open PR creation in browser (do NOT create directly). Detach so the
   tool call returns immediately instead of blocking on the browser:
   \`\`\`bash
   nohup gh pr create --web --title "<title>" --body "<body>" >/dev/null 2>&1 &
   \`\`\`
`.trim();

// Anything larger than this is probably noise (lockfiles, generated code).
// The agent can re-run git diff with pathspecs if it needs more.
const DIFF_LIMIT = 200_000;

// GitHub:  .github/PULL_REQUEST_TEMPLATE.md, docs/..., root
// Gitea:   same as GitHub plus .gitea/ (mirrors .github/)
// GitLab:  .gitlab/merge_request_templates/Default.md
// All forges accept both upper- and lowercase filenames, so match
// case-insensitively against readdir() instead of hardcoding every variant.
const TEMPLATE_LOCATIONS: Array<[dir: string, name: string]> = [
  [".github", "pull_request_template.md"],
  [".gitea", "pull_request_template.md"],
  [".gitlab/merge_request_templates", "default.md"],
  ["docs", "pull_request_template.md"],
  [".", "pull_request_template.md"],
];

function findPrTemplate(
  repoRoot: string,
): { path: string; body: string } | undefined {
  for (const [dir, name] of TEMPLATE_LOCATIONS) {
    let entries: string[];
    try {
      entries = readdirSync(join(repoRoot, dir));
    } catch {
      continue;
    }
    const hit = entries.find((e) => e.toLowerCase() === name);
    if (hit) {
      const path = join(dir, hit);
      try {
        return { path, body: readFileSync(join(repoRoot, path), "utf8") };
      } catch {
        // unreadable (permissions, broken symlink) — keep looking
      }
    }
  }
  return undefined;
}

/**
 * Pick the diff base. Prefer remote-tracking refs over local branch names —
 * local main is often weeks behind on long-lived feature branches, which
 * makes the diff useless (see the 209-commit case that prompted this).
 *
 *   1. origin/<workmux-base>  if workmux set a base and origin has it
 *   2. origin/HEAD            (whatever the remote calls its default)
 *   3. <workmux-base>         local fallback if origin is unreachable
 *   4. main                   last resort
 */
async function resolveBase(exec: Exec, cur: string): Promise<string> {
  const wb = await workmuxBase(exec, cur);

  if (wb) {
    const remote = await git(exec, [
      "rev-parse",
      "--verify",
      "-q",
      `origin/${wb}`,
    ]);
    if (remote) return `origin/${wb}`;
  }

  const originHead = await git(exec, [
    "symbolic-ref",
    "--short",
    "refs/remotes/origin/HEAD",
  ]);
  if (originHead) return originHead;

  return wb || "main";
}

export default function (pi: ExtensionAPI) {
  pi.registerCommand("open-pr", {
    description: "Write a PR description and open PR creation in browser",
    handler: async (_args, _ctx) => {
      const [cur, repoRoot] = await Promise.all([
        currentBranch(pi.exec),
        git(pi.exec, ["rev-parse", "--show-toplevel"]),
      ]);
      const baseBranch = await resolveBase(pi.exec, cur);

      const [status, log, diff] = await Promise.all([
        pi.exec("git", ["status"], { timeout: 5000 }),
        pi.exec("git", ["log", `${baseBranch}..HEAD`, "--format=%s%n%n%b"], {
          timeout: 5000,
        }),
        pi.exec("git", ["diff", `${baseBranch}...HEAD`, "--stat", "-p"], {
          timeout: 10000,
        }),
      ]);

      const template = findPrTemplate(repoRoot);

      let diffOut = diff.stdout;
      if (diffOut.length > DIFF_LIMIT) {
        diffOut = diffOut.slice(0, DIFF_LIMIT) +
          `\n\n[... diff truncated at ${DIFF_LIMIT} bytes, re-run git diff with pathspecs if needed ...]`;
      }

      const templateSection = template
        ? [
          `Found \`${template.path}\` — use this instead of the default template:`,
          "",
          "```markdown",
          template.body.trim(),
          "```",
        ].join("\n")
        : "(no PR template found in repo, use the default above)";

      const gitContext = [
        "## Current git state",
        "",
        `Current branch: \`${cur}\``,
        `Base branch: \`${baseBranch}\``,
        "",
        "### git status",
        "```",
        status.stdout.trim(),
        "```",
        "",
        `### git log ${baseBranch}..HEAD`,
        "```",
        log.stdout.trim() || "(no commits ahead of base)",
        "```",
        "",
        "### Repo PR template",
        templateSection,
        "",
        `### git diff ${baseBranch}...HEAD`,
        "```diff",
        diffOut.trim() || "(no diff)",
        "```",
      ].join("\n");

      pi.sendUserMessage(`${OPEN_PR_PROMPT}\n\n${gitContext}`, {
        deliverAs: "followUp",
      });
    },
  });
}
