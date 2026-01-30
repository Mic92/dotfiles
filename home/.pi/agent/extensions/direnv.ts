/**
 * Direnv Extension
 *
 * Loads direnv environment variables on session start and after each bash
 * command. This mimics how the shell hook works — it runs after every command
 * to pick up any .envrc changes from cd, git checkout, etc.
 *
 * - Only one direnv process runs at a time — subsequent requests wait for
 *   the current one to finish before spawning a new one.
 * - Blocks for up to 10s so fast completions are handled inline. If direnv
 *   takes longer, control returns and the process finishes in the background.
 * - Status bar shows: "direnv …" (running), "direnv ✓" (loaded), "direnv ✗" (error).
 *
 * Requirements:
 *   - direnv installed and in PATH
 *   - .envrc must be allowed (run `direnv allow` in your shell first)
 */

import { spawn } from "node:child_process";
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  let pending: Promise<void> | null = null;

  async function loadDirenv(cwd: string, ctx: ExtensionContext) {
    // Wait for any in-flight direnv process before starting a new one
    if (pending) {
      await pending;
    }

    if (ctx.hasUI) {
      ctx.ui.setStatus("direnv", ctx.ui.theme.fg("warning", "direnv …"));
    }
    pending = runDirenv(cwd, ctx);
    await pending;
    pending = null;
  }

  function runDirenv(cwd: string, ctx: ExtensionContext) {
    const proc = spawn("direnv", ["export", "json"], {
      cwd,
      stdio: ["pipe", "pipe", "pipe"],
    });

    let stdout = "";
    proc.stdout.on("data", (chunk: Buffer) => {
      stdout += chunk.toString();
    });

    const done = new Promise<void>((resolve) => {
      proc.on("close", (code) => {
        if (code !== 0) {
          if (ctx.hasUI) {
            ctx.ui.setStatus(
              "direnv",
              ctx.ui.theme.fg("error", "direnv ✗"),
            );
          }
          resolve();
          return;
        }

        applyEnv(stdout, ctx);
        // No env changes means everything is in sync — still a success
        if (ctx.hasUI && !stdout.trim()) {
          ctx.ui.setStatus("direnv", ctx.ui.theme.fg("success", "direnv ✓"));
        }
        resolve();
      });

      proc.on("error", () => {
        if (ctx.hasUI) {
          ctx.ui.setStatus("direnv", ctx.ui.theme.fg("error", "direnv ✗"));
        }
        resolve();
      });
    });

    // Block for up to 10s so fast completions are handled inline.
    // If still running, return and let the process finish in the
    // background — status will update when it exits.
    const timeout = new Promise<void>((resolve) => setTimeout(resolve, 10_000));
    return Promise.race([done, timeout]);
  }

  function applyEnv(output: string, ctx: ExtensionContext) {
    if (!output.trim()) return;

    try {
      const env = JSON.parse(output);
      let loadedCount = 0;
      for (const [key, value] of Object.entries(env)) {
        if (value === null) {
          delete process.env[key];
        } else {
          process.env[key] = value as string;
          loadedCount++;
        }
      }

      if (ctx.hasUI && loadedCount > 0) {
        ctx.ui.setStatus("direnv", ctx.ui.theme.fg("success", "direnv ✓"));
      }
    } catch {
      if (ctx.hasUI) {
        ctx.ui.setStatus("direnv", ctx.ui.theme.fg("error", "direnv ✗"));
      }
    }
  }

  pi.on("session_start", async (_event, ctx) => {
    await loadDirenv(ctx.cwd, ctx);
  });

  // Run direnv after every bash command to pick up .envrc changes
  // This handles: cd to new dir, git checkout, direnv allow, etc.
  pi.on("tool_result", async (event, ctx) => {
    if (event.toolName !== "bash") return;
    await loadDirenv(ctx.cwd, ctx);
  });
}
