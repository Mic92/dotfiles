/**
 * tuicr Extension
 *
 * Registers a `tuicr` tool that the LLM can call to launch the tuicr TUI
 * for interactive code review. Suspends the pi TUI and runs tuicr full-screen
 * with terminal access. Captures exported instructions via --stdout.
 */

import { execSync, spawnSync } from "node:child_process";
import { closeSync, openSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "tuicr",
    label: "tuicr",
    description: "Launch code review. Get feedback from the User.",
    parameters: Type.Object({
      directory: Type.Optional(
        Type.String({ description: "Git repo path (default: cwd)" }),
      ),
      revisions: Type.Optional(
        Type.String({ description: "Commit range (e.g. HEAD~3..HEAD)" }),
      ),
    }),

    async execute(toolCallId, params, signal, onUpdate, ctx) {
      // Check tuicr is installed
      try {
        execSync("command -v tuicr", { stdio: "ignore" });
      } catch {
        return {
          content: [
            {
              type: "text",
              text:
                "tuicr is not installed. Install via: nix run github:agavra/tuicr",
            },
          ],
        };
      }

      const targetDir = params.directory || ctx.cwd;

      // Check it's a git repo
      try {
        execSync("git rev-parse --git-dir", {
          cwd: targetDir,
          stdio: "ignore",
        });
      } catch {
        return {
          content: [{
            type: "text",
            text: `Not a git repository: ${targetDir}`,
          }],
        };
      }

      if (!ctx.hasUI) {
        return {
          content: [{
            type: "text",
            text: "(tuicr requires interactive TUI mode)",
          }],
        };
      }

      // Build tuicr args
      const tuicrArgs: string[] = ["--stdout"];
      if (params.revisions) {
        tuicrArgs.push("-r", params.revisions);
      }

      // --stdout writes export to stdout; redirect to file so TUI uses stderr
      const outputFile = join(tmpdir(), `tuicr-${Date.now()}.md`);

      // Run tuicr with full terminal access
      try {
        await ctx.ui.custom<number | null>((tui, _theme, _kb, done) => {
          tui.stop();
          process.stdout.write("\x1b[2J\x1b[H");

          const fd = openSync(outputFile, "w");
          try {
            const result = spawnSync("tuicr", tuicrArgs, {
              stdio: ["inherit", fd, "inherit"],
              env: process.env,
              cwd: targetDir,
            });
            tui.start();
            tui.requestRender(true);
            done(result.status);
          } finally {
            closeSync(fd);
          }

          return { render: () => [], invalidate: () => {} };
        });
      } catch {
        // ui.custom can throw on cancel
      }

      // Read captured instructions, stripping any terminal escape sequences
      // Covers CSI (ESC[..X), OSC (ESC]...ST), and ESC+char sequences
      const ansiRegex =
        /(?:\x1b\].*?(?:\x1b\\|\x07)|\x1b[\[()#;?]*[0-9;]*[A-Za-z@`\^\[\]{}|~=><]|\x9b[0-9;]*[A-Za-z@`\^\[\]{}|~=><])/g;
      let instructions = "";
      try {
        instructions = readFileSync(outputFile, "utf-8").replace(ansiRegex, "")
          .trim();
      } catch {}
      try {
        rmSync(outputFile, { force: true });
      } catch {}

      if (instructions) {
        return {
          content: [
            {
              type: "text",
              text: `Review completed:\n${instructions}`,
            },
          ],
        };
      }

      return {
        content: [
          {
            type: "text",
            text:
              "Review completed. No instructions were exported. The user may paste instructions.",
          },
        ],
      };
    },
  });
}
