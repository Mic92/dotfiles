/**
 * Shared git helpers for pi extensions.
 *
 * Several commands (commit/merge/rebase/review/open-pr) need the same
 * "what branch am I on" and "what is my base branch" answers. Centralising
 * them keeps the fallback order consistent and avoids the .code/.exitCode
 * drift that already crept in once.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export type Exec = ExtensionAPI["exec"];

/** Run git, return trimmed stdout on success or "" on failure. */
export async function git(
  exec: Exec,
  args: string[],
  timeout = 5000,
): Promise<string> {
  const r = await exec("git", args, { timeout });
  return r.code === 0 ? r.stdout.trim() : "";
}

export async function currentBranch(exec: Exec): Promise<string> {
  return git(exec, ["branch", "--show-current"]);
}

/** Raw `branch.<cur>.workmux-base` config value, or "". */
export async function workmuxBase(exec: Exec, cur: string): Promise<string> {
  if (!cur) return "";
  return git(exec, [
    "config",
    "--local",
    "--get",
    `branch.${cur}.workmux-base`,
  ]);
}

/**
 * Local base branch for rebase/diff: workmux-base → main → master.
 * Never touches the remote — callers that want origin/* should resolve
 * that themselves (see open-pr).
 */
export async function localBase(exec: Exec, cur?: string): Promise<string> {
  const branch = cur ?? (await currentBranch(exec));
  const wb = await workmuxBase(exec, branch);
  if (wb) return wb;
  const hasMain = await git(exec, ["rev-parse", "--verify", "-q", "main"]);
  return hasMain ? "main" : "master";
}

// pi's loader treats every *.ts in extensions/ as an extension and errors on
// modules without a default export. This file is a helper library, so export
// a no-op factory to keep the loader quiet.
export default function (_pi: ExtensionAPI) {}
