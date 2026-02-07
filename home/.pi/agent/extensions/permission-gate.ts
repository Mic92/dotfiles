/**
 * Permission Gate Extension
 *
 * Prompts for confirmation before running potentially dangerous bash commands.
 * Based on upstream example: examples/extensions/permission-gate.ts
 *
 * Toggle with /permission-gate command.
 * Status bar shows "gate ■" when active.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  let enabled = true;

  const dangerousPatterns: { pattern: RegExp; label: string }[] = [
    { pattern: /\brm\s+(-[^\s]*r|--recursive)/, label: "recursive delete" },
    { pattern: /\bsudo\b/, label: "sudo" },
    { pattern: /\bssh\b/, label: "ssh" },
    { pattern: /\bchmod\b.*777/, label: "world-writable permissions" },
    { pattern: />\s*\/dev\/[sh]d[a-z]/, label: "raw device redirect" },
    { pattern: /\bgit\s+push\s+.*(-f\b|--force\b)/, label: "force push" },
    { pattern: /\bgit\s+reset\s+--hard\b/, label: "hard reset" },
    { pattern: /\bgit\s+clean\s+-[^\s]*f/, label: "git clean" },
    {
      pattern: /\bgit\s+checkout\s+(\S+\s+)?--\s/,
      label: "git checkout (reset files)",
    },
    {
      pattern: /\bgit\s+checkout\s+\.\s*($|[;&|])/,
      label: "git checkout (reset all files)",
    },
    { pattern: /\bgit\s+restore\b/, label: "git restore" },
    { pattern: /\bclan\s+machines\s+update\b/, label: "deploy to machine" },
    { pattern: /\bcurl\b.*\|\s*(ba)?sh\b/, label: "pipe curl to shell" },
    { pattern: /\bwget\b.*\|\s*(ba)?sh\b/, label: "pipe wget to shell" },
    { pattern: /\bgh\s+issue\s+create\b/, label: "create GitHub issue" },
    {
      pattern: /\bgh\s+issue\s+(close|delete|edit|comment)\b/,
      label: "modify GitHub issue",
    },
    { pattern: /\bgh\s+pr\s+create\b/, label: "create GitHub PR" },
    {
      pattern: /\bgh\s+pr\s+(close|merge|edit|comment|review)\b/,
      label: "modify GitHub PR",
    },
    {
      pattern: /\bgh\s+repo\s+(create|delete|rename|archive)\b/,
      label: "modify GitHub repo",
    },
    {
      pattern: /\bgh\s+release\s+(create|delete|edit)\b/,
      label: "modify GitHub release",
    },
    {
      pattern: /\btea\s+(issue|pr)\s+create\b/,
      label: "create Gitea issue/PR",
    },
    {
      pattern: /\btea\s+(issue|pr)\s+(close|edit)\b/,
      label: "modify Gitea issue/PR",
    },
    { pattern: /\btea\s+comment\b/, label: "Gitea comment" },
    { pattern: /\bmsmtp\b/, label: "send email" },
  ];

  pi.registerCommand("permission-gate", {
    description:
      "Toggle permission gate — confirm dangerous commands before running",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) return;

      enabled = !enabled;
      if (enabled) {
        ctx.ui.setStatus(
          "permission-gate",
          ctx.ui.theme.fg("warning", "gate ■"),
        );
        ctx.ui.notify(
          "Permission gate enabled — dangerous commands require approval",
          "info",
        );
      } else {
        ctx.ui.setStatus("permission-gate", undefined);
        ctx.ui.notify("Permission gate disabled", "info");
      }
    },
  });

  // Show status on startup
  pi.on("session_start", async (_event, ctx) => {
    if (enabled && ctx.hasUI) {
      ctx.ui.setStatus("permission-gate", ctx.ui.theme.fg("warning", "gate ■"));
    }
  });

  pi.on("tool_call", async (event, ctx) => {
    if (!enabled) return undefined;
    if (event.toolName !== "bash") return undefined;

    const command = event.input.command as string;
    const matched = dangerousPatterns.filter((p) => p.pattern.test(command));

    if (matched.length > 0) {
      const labels = matched.map((m) => m.label).join(", ");

      if (!ctx.hasUI) {
        return {
          block: true,
          reason:
            `Dangerous command blocked (${labels}) — no UI for confirmation`,
        };
      }

      pi.events.emit("permission-gate:waiting");

      const choice = await ctx.ui.select(
        `⚠️  Dangerous command detected (${labels}):\n\n  ${command}\n\nAllow?`,
        ["Yes", "No"],
      );

      pi.events.emit("permission-gate:resolved");

      if (choice !== "Yes") {
        return { block: true, reason: `Blocked by user (${labels})` };
      }
    }

    return undefined;
  });
}
