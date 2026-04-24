/**
 * proc-title — put the current git branch into pi's process title.
 *
 * pi sets `process.title = "pi"` once at startup, so every running
 * instance looks identical in `ps`, `htop`, `pgrep`, tmux pane titles
 * derived from argv[0], etc. When juggling several worktrees that is
 * useless. This rewrites the title to `pi [<branch>]` (or `pi [@<sha>]`
 * for detached HEAD) once on startup so the right instance can be found
 * from outside the terminal.
 */

import { execFile } from "node:child_process";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

function git(args: string[], cwd: string): Promise<string | null> {
  return new Promise((resolve) => {
    execFile("git", args, { cwd, timeout: 2000 }, (err, stdout) => {
      resolve(err ? null : stdout.trim());
    });
  });
}

export default function procTitle(pi: ExtensionAPI) {
  pi.on("session_start", async (_event, ctx) => {
    const branch = await git(["branch", "--show-current"], ctx.cwd);
    if (branch === null) return; // not a git repo / git missing
    const label = branch ||
      (await git(["rev-parse", "--short", "HEAD"], ctx.cwd).then((s) =>
        s ? `@${s}` : null
      ));
    if (label) process.title = `pi [${label}]`;
  });
}
