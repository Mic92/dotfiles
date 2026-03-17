/**
 * Cokain — Anthropic fast mode (~2.5x faster output on claude-opus-4-6).
 * Toggle with /cokain. State persisted across sessions. Default: enabled.
 * Patches fetch to inject speed:"fast" + beta header into Messages API requests.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { homedir } from "node:os";

const STATE_FILE = join(homedir(), ".pi", "state", "cokain.json");

function load(): boolean {
  try {
    return JSON.parse(readFileSync(STATE_FILE, "utf-8")).enabled ?? true;
  } catch {}
  return true;
}

function save(v: boolean) {
  mkdirSync(dirname(STATE_FILE), { recursive: true });
  writeFileSync(STATE_FILE, JSON.stringify({ enabled: v }), "utf-8");
}

export default function cokain(pi: ExtensionAPI) {
  let enabled = load();

  const setStatus = (ctx: {
    ui: {
      setStatus: (id: string, text: string | undefined) => void;
      theme: any;
    };
  }) =>
    ctx.ui.setStatus(
      "cokain",
      enabled ? ctx.ui.theme.fg("warning", "fast ⚡") : undefined,
    );

  pi.on("session_start", async (_ev, ctx) => setStatus(ctx));

  pi.registerCommand("cokain", {
    description: "Toggle Anthropic fast mode for claude-opus-4-6",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) return;
      enabled = !enabled;
      save(enabled);
      setStatus(ctx);
      ctx.ui.notify(
        enabled ? "Fast mode enabled" : "Fast mode disabled",
        "info",
      );
    },
  });

  const originalFetch = globalThis.fetch;
  globalThis.fetch = async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (!enabled || !init?.body) return originalFetch(input, init);

    const url = typeof input === "string"
      ? input
      : input instanceof URL
      ? input.href
      : input.url;
    if (!url.includes("/v1/messages") || !url.includes("anthropic")) {
      return originalFetch(input, init);
    }

    const bodyStr = typeof init.body === "string" ? init.body : undefined;
    if (!bodyStr) {
      return originalFetch(input, init);
    }

    try {
      const body = JSON.parse(bodyStr);
      if (!body.model?.startsWith("claude-opus-4-6")) {
        return originalFetch(input, init);
      }

      body.speed = "fast";
      const headers = new Headers(init.headers);
      const beta = headers.get("anthropic-beta") || "";
      if (!beta.includes("fast-mode-2026-02-01")) {
        headers.set(
          "anthropic-beta",
          beta ? `${beta},fast-mode-2026-02-01` : "fast-mode-2026-02-01",
        );
      }

      return originalFetch(input, {
        ...init,
        body: JSON.stringify(body),
        headers,
      });
    } catch {
      return originalFetch(input, init);
    }
  };

  pi.on("session_shutdown", async () => {
    globalThis.fetch = originalFetch;
  });
}
