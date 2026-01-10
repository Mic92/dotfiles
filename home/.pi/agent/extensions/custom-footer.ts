/**
 * Custom Footer Extension - shows working directory, git branch, model, and context usage
 */

import type { AssistantMessage } from "@mariozechner/pi-ai";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@mariozechner/pi-tui";

export default function (pi: ExtensionAPI) {
  pi.on("session_start", async (_event, ctx) => {
    ctx.ui.setFooter((tui, theme, footerData) => {
      const unsub = footerData.onBranchChange(() => tui.requestRender());

      return {
        dispose: unsub,
        invalidate() {},
        render(width: number): string[] {
          // Find last non-aborted assistant message for context calculation
          const messages = ctx.sessionManager.getBranch()
            .filter((e): e is { type: "message"; message: AssistantMessage } =>
              e.type === "message" && e.message.role === "assistant"
            )
            .map((e) => e.message)
            .filter((m) => m.stopReason !== "aborted");

          const lastMessage = messages[messages.length - 1];

          // Calculate context tokens from last message
          const contextTokens = lastMessage
            ? lastMessage.usage.input +
              lastMessage.usage.output +
              lastMessage.usage.cacheRead +
              lastMessage.usage.cacheWrite
            : 0;

          const contextWindow = ctx.model?.contextWindow || 0;

          const fmt = (n: number) => {
            if (n < 1000) return n.toString();
            if (n < 10000) return `${(n / 1000).toFixed(1)}k`;
            if (n < 1000000) return `${Math.round(n / 1000)}k`;
            return `${(n / 1000000).toFixed(1)}M`;
          };

          // Get git branch
          const branch = footerData.getGitBranch();
          const branchStr = branch
            ? theme.fg("dim", " │ ") + theme.fg("success", " ") +
              theme.fg("accent", branch)
            : "";

          // Get working directory (shortened)
          const cwd = ctx.cwd;
          const home = process.env.HOME || "";
          const shortCwd = home && cwd.startsWith(home)
            ? "~" + cwd.slice(home.length)
            : cwd;

          // Build left side: cwd + branch
          const left = theme.fg("muted", shortCwd) + branchStr;

          // Build right side: context usage + model
          const modelId = ctx.model?.id || "no-model";

          // Colorize context based on usage percentage
          const percentValue = contextWindow > 0
            ? (contextTokens / contextWindow) * 100
            : 0;
          let contextColor: "success" | "warning" | "error";
          if (percentValue > 90) {
            contextColor = "error";
          } else if (percentValue > 70) {
            contextColor = "warning";
          } else {
            contextColor = "success";
          }

          const contextDisplay = theme.fg(contextColor, fmt(contextTokens)) +
            theme.fg("dim", `/`) + theme.fg("accent", fmt(contextWindow));
          const right = contextDisplay + theme.fg("dim", " │ ") +
            theme.fg("toolTitle", modelId);

          const pad = " ".repeat(
            Math.max(1, width - visibleWidth(left) - visibleWidth(right)),
          );
          return [truncateToWidth(left + pad + right, width)];
        },
      };
    });
  });
}
