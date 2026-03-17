/**
 * Fast Mode Extension for Claude Opus 4.6
 *
 * Adds support for Anthropic's "fast" speed parameter, which provides
 * ~2.5x faster output token generation on claude-opus-4-6.
 *
 * Toggle with /fast-mode command or Ctrl+Shift+F shortcut.
 * Status bar shows "fast ⚡" when active.
 *
 * Patches global fetch to inject the required beta header and speed
 * parameter into Anthropic Messages API requests when enabled.
 *
 * Reference: https://platform.claude.com/docs/en/build-with-claude/fast-mode
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function fastMode(pi: ExtensionAPI) {
  let enabled = false;

  function updateStatus(
    ctx: {
      ui: {
        setStatus: (id: string, text: string | undefined) => void;
        theme: any;
      };
    },
  ) {
    if (enabled) {
      ctx.ui.setStatus("fast-mode", ctx.ui.theme.fg("warning", "fast ⚡"));
    } else {
      ctx.ui.setStatus("fast-mode", undefined);
    }
  }

  // Toggle command
  pi.registerCommand("fast-mode", {
    description:
      "Toggle Anthropic fast mode for Claude Opus 4.6 (~2.5x faster output)",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) return;
      enabled = !enabled;
      updateStatus(ctx);
      if (enabled) {
        ctx.ui.notify(
          'Fast mode enabled — claude-opus-4-6 will use speed: "fast"',
          "info",
        );
      } else {
        ctx.ui.notify("Fast mode disabled", "info");
      }
    },
  });

  // Patch global fetch to inject fast mode header + body param
  const originalFetch = globalThis.fetch;
  globalThis.fetch = async function patchedFetch(
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> {
    if (!enabled || !init?.body) {
      return originalFetch(input, init);
    }

    // Only intercept Anthropic Messages API calls
    const url = typeof input === "string"
      ? input
      : input instanceof URL
      ? input.href
      : input.url;
    if (!url.includes("/v1/messages") || !url.includes("anthropic")) {
      return originalFetch(input, init);
    }

    // Only inject for claude-opus-4-6
    const bodyStr = typeof init.body === "string" ? init.body : undefined;
    if (!bodyStr) {
      return originalFetch(input, init);
    }

    try {
      const body = JSON.parse(bodyStr);
      if (!body.model?.startsWith("claude-opus-4-6")) {
        return originalFetch(input, init);
      }

      // Inject speed: "fast" into the body
      body.speed = "fast";
      const newBody = JSON.stringify(body);

      // Inject fast-mode beta into the anthropic-beta header
      const headers = new Headers(init.headers);
      const existingBeta = headers.get("anthropic-beta") || "";
      if (!existingBeta.includes("fast-mode-2026-02-01")) {
        const newBeta = existingBeta
          ? `${existingBeta},fast-mode-2026-02-01`
          : "fast-mode-2026-02-01";
        headers.set("anthropic-beta", newBeta);
      }

      return originalFetch(input, {
        ...init,
        body: newBody,
        headers,
      });
    } catch {
      // JSON parse failed, pass through
      return originalFetch(input, init);
    }
  };

  // Restore original fetch on shutdown
  pi.on("session_shutdown", async () => {
    globalThis.fetch = originalFetch;
  });
}
