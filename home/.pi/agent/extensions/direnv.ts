/**
 * Direnv Extension
 *
 * Loads direnv environment variables on session start and after each bash
 * command. This mimics how the shell hook works - it runs after every command
 * to pick up any .envrc changes from cd, git checkout, etc.
 *
 * Requirements:
 *   - direnv installed and in PATH
 *   - .envrc must be allowed (run `direnv allow` in your shell first)
 */

import { execSync } from "node:child_process";
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  function loadDirenv(cwd: string, ctx: ExtensionContext) {
    try {
      const output = execSync("direnv export json", {
        cwd,
        encoding: "utf-8",
        stdio: ["pipe", "pipe", "pipe"],
      });

      if (!output.trim()) {
        // No env changes - direnv is available but no .envrc or already loaded
        return;
      }

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
      // direnv not available or .envrc blocked/failed - show error indicator
      if (ctx.hasUI) {
        ctx.ui.setStatus("direnv", ctx.ui.theme.fg("error", "direnv ✗"));
      }
    }
  }

  pi.on("session_start", async (_event, ctx) => {
    loadDirenv(ctx.cwd, ctx);
  });

  // Run direnv after every bash command to pick up .envrc changes
  // This handles: cd to new dir, git checkout, direnv allow, etc.
  pi.on("tool_result", async (event, ctx) => {
    if (event.toolName !== "bash") return;
    loadDirenv(ctx.cwd, ctx);
  });
}
