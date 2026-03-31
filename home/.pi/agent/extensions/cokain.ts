/**
 * Cokain — Anthropic fast mode, default model, + beta header fix.
 *
 * Patches fetch on all Anthropic Messages API requests to:
 * - Always append context-1m-2025-08-07 beta (works around pi's
 *   mergeHeaders() Object.assign bug that clobbers provider betas).
 * - When /cokain is toggled on: inject speed:"fast" + fast-mode beta
 *   for opus models (~2.5x faster output).
 *
 * If cokain.json has a modelId, the fetch interceptor rewrites the
 * model field in outgoing API requests. Pi sees the standard model
 * name; only the wire request uses the private/beta model ID.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { homedir } from "node:os";

const STATE_FILE = join(homedir(), ".pi", "state", "cokain.json");

interface State {
  enabled?: boolean;
  modelId?: string;
}

function loadState(): State {
  try {
    return JSON.parse(readFileSync(STATE_FILE, "utf-8"));
  } catch {}
  return {};
}

function saveState(s: State) {
  mkdirSync(dirname(STATE_FILE), { recursive: true });
  writeFileSync(STATE_FILE, JSON.stringify(s), "utf-8");
}

function ensureBeta(headers: Headers, flag: string) {
  const cur = headers.get("anthropic-beta") || "";
  if (!cur.includes(flag)) {
    headers.set("anthropic-beta", cur ? `${cur},${flag}` : flag);
  }
}

const initialState = loadState();

export default function cokain(pi: ExtensionAPI) {
  let state = initialState;

  const setStatus = (ctx: {
    ui: {
      setStatus: (id: string, text: string | undefined) => void;
      theme: any;
    };
  }) =>
    ctx.ui.setStatus(
      "cokain",
      state.enabled ? ctx.ui.theme.fg("warning", "fast ⚡") : undefined,
    );

  pi.on("session_start", async (_ev, ctx) => setStatus(ctx));

  pi.registerCommand("cokain", {
    description: "Toggle Anthropic fast mode for claude-opus-4-6",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) return;
      state.enabled = !state.enabled;
      saveState(state);
      setStatus(ctx);
      ctx.ui.notify(
        state.enabled ? "Fast mode enabled" : "Fast mode disabled",
        "info",
      );
    },
  });

  const originalFetch = globalThis.fetch;
  globalThis.fetch = async (
    input: RequestInfo | URL,
    init?: RequestInit,
  ): Promise<Response> => {
    if (!init?.body) return originalFetch(input, init);
    const url = typeof input === "string"
      ? input
      : input instanceof URL
      ? input.href
      : input.url;
    if (!url.includes("/v1/messages") || !url.includes("anthropic")) {
      return originalFetch(input, init);
    }
    const bodyStr = typeof init.body === "string" ? init.body : undefined;
    if (!bodyStr) return originalFetch(input, init);

    try {
      const body = JSON.parse(bodyStr);
      const headers = new Headers(init.headers);

      // Rewrite model ID if configured (pi sees the standard name,
      // wire request uses the private/beta model ID).
      if (state.modelId && body.model) {
        body.model = state.modelId;
        if (body.thinking?.type === "disabled") {
          delete body.thinking;
        }
      }

      ensureBeta(headers, "context-1m-2025-08-07");

      if (
        state.enabled &&
        body.model?.startsWith("claude-opus-4-6")
      ) {
        body.speed = "fast";
        ensureBeta(headers, "fast-mode-2026-02-01");
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
