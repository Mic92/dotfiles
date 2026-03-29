/**
 * nostr-walkie — bridge pi sessions to Nostr DMs.
 *
 * Port of Aldo Borrero's walkie (Telegram) to NIP-17 private DMs.
 *
 *  pi → Nostr:  agent_end pushes the last assistant text + run stats as a DM
 *               <nostr-file>…</nostr-file> tags in the reply trigger file sends
 *  Nostr → pi:  incoming DMs become user prompts (followUp when busy)
 *               kind-15 file rumors become image attachments / path refs
 *               /abort steers + ctx.abort(), /status replies with agent state
 *
 * No live streaming — NIP-17 has no draft/typing equivalent and spamming
 * DMs per-token would be obnoxious.
 */

import { readFileSync } from "node:fs";
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import {
  type Config,
  loadConfig,
  type PrivateKeySource,
  resolvePrivateKey,
  saveConfig,
} from "./config.ts";
import {
  deriveKeys,
  generateKeypair,
  type Inbound,
  NostrBridge,
  npubEncode,
  parsePubkey,
} from "./nostr.ts";

/** Per-run counters reset on agent_start. */
interface RunStats {
  turns: number;
  filesChanged: number;
  startMs: number;
}

export default function nostrWalkie(pi: ExtensionAPI) {
  let config: Config | undefined;
  let bridge: NostrBridge | undefined;
  let busy = false;
  let stats: RunStats = { turns: 0, filesChanged: 0, startMs: 0 };
  /** rumor.id of the DM that triggered the current run — for ✅ reaction. */
  let pendingReact: string | undefined;

  // Inbound text debounce: merge rapid consecutive DMs so a user typing
  // three short lines doesn't fire three agent runs.
  let debounceBuf = "";
  let debounceIds: string[] = [];
  let debounceTimer: ReturnType<typeof setTimeout> | undefined;
  const DEBOUNCE_MS = 3000;

  const isActive = () => !!bridge && !!config?.enabled;

  function flushInbound() {
    const text = debounceBuf.trim();
    const ids = debounceIds;
    debounceBuf = "";
    debounceIds = [];
    debounceTimer = undefined;
    if (!text) return;
    pendingReact = ids[ids.length - 1];
    if (busy) {
      pi.sendUserMessage(text, { deliverAs: "followUp" });
    } else {
      pi.sendUserMessage(text);
    }
  }

  function queueInbound(id: string, text: string) {
    debounceBuf = debounceBuf ? `${debounceBuf}\n${text}` : text;
    debounceIds.push(id);
    if (debounceTimer) clearTimeout(debounceTimer);
    debounceTimer = setTimeout(flushInbound, DEBOUNCE_MS);
  }

  /** Handle a freshly-unwrapped inbound event from the controller. */
  function onInbound(ctx: ExtensionContext, m: Inbound): void {
    // 👀 ack so the sender knows it landed
    bridge?.sendReaction(m.id, "👀");

    if (m.kind === "file") {
      pendingReact = m.id;
      const content = buildFilePrompt(m.path, m.mimeType);
      if (busy) pi.sendUserMessage(content, { deliverAs: "followUp" });
      else pi.sendUserMessage(content);
      return;
    }

    const trimmed = m.text.trim();
    if (trimmed === "/abort") {
      pi.sendUserMessage("User requested abort via Nostr.", {
        deliverAs: "steer",
      });
      ctx.abort();
      bridge?.sendDM("⏹️ aborted");
      return;
    }
    if (trimmed === "/status") {
      const state = busy ? "busy" : "idle";
      bridge?.sendDM(`📊 ${state} · cwd ${ctx.cwd}`);
      return;
    }
    queueInbound(m.id, trimmed);
  }

  function startBridge(ctx: ExtensionContext): void {
    if (!config) return;
    let keys;
    try {
      const sk = resolvePrivateKey(config.privateKey);
      keys = deriveKeys(sk, config.recipientPubkey);
    } catch (e) {
      if (ctx.hasUI) {
        ctx.ui.notify(
          `nostr-walkie: bad key config: ${(e as Error).message}`,
          "error",
        );
      } else {
        console.error("nostr-walkie:", (e as Error).message);
      }
      return;
    }
    bridge = new NostrBridge(
      keys,
      config.relays,
      config.blossomServers,
      (level, msg) => {
        if (level === "warn") console.warn(msg);
        else if (level === "info") console.info(msg);
        // debug dropped
      },
    );
    bridge.start((m) => onInbound(ctx, m));
    if (ctx.hasUI) {
      ctx.ui.setStatus("nostr-walkie", `📡 ${bridge.npub().slice(0, 12)}…`);
    }
  }

  // ─── lifecycle ────────────────────────────────────────────────────────

  pi.on("session_start", async (_ev, ctx) => {
    config = loadConfig(ctx.cwd);
    if (!config) {
      if (ctx.hasUI) {
        ctx.ui.notify(
          "nostr-walkie: not configured — run /nostr-walkie setup",
          "info",
        );
      }
      return;
    }
    if (!config.enabled) return;
    startBridge(ctx);
    bridge?.sendDM(`🟢 pi session started in ${ctx.cwd}`);
  });

  pi.on("session_shutdown", async () => {
    if (debounceTimer) clearTimeout(debounceTimer);
    if (isActive()) {
      // Best-effort — fire into the queue, but don't block shutdown.
      bridge?.sendDM("🔴 pi session ended");
    }
    bridge?.stop();
    bridge = undefined;
  });

  pi.on("agent_start", async () => {
    busy = true;
    stats = { turns: 0, filesChanged: 0, startMs: Date.now() };
  });

  pi.on("turn_start", async () => {
    stats.turns++;
  });

  pi.on("tool_result", async (ev) => {
    if (ev.toolName === "edit" || ev.toolName === "write") {
      if (!ev.isError) stats.filesChanged++;
    }
  });

  pi.on("agent_end", async (ev) => {
    busy = false;
    if (!isActive()) return;

    const raw = extractLastAssistantText(ev.messages) || "(no text output)";
    const { text, files } = stripFileTags(raw);
    const elapsed = ((Date.now() - stats.startMs) / 1000).toFixed(1);
    const footer = `\n\n— ${stats.turns} turn${stats.turns === 1 ? "" : "s"}` +
      ` · ${stats.filesChanged} file${stats.filesChanged === 1 ? "" : "s"}` +
      ` · ${elapsed}s`;

    bridge?.sendDM(text + footer);
    for (const f of files) {
      try {
        await bridge?.sendFile(f);
      } catch (e) {
        console.error(
          `nostr-walkie: sendFile ${f} failed:`,
          (e as Error).message,
        );
      }
    }
    if (pendingReact) {
      bridge?.sendReaction(pendingReact, "✅");
      pendingReact = undefined;
    }
  });

  // ─── /nostr-walkie command ────────────────────────────────────────────

  pi.registerCommand("nostr-walkie", {
    description: "Configure or toggle the Nostr DM bridge",
    handler: async (args, ctx) => {
      const sub = (args || "").trim().split(/\s+/)[0];

      if (sub === "setup") {
        if (!ctx.hasUI) {
          ctx.ui.notify("nostr-walkie: setup requires interactive UI", "error");
          return;
        }
        const choice = (await ctx.ui.input(
          "pi private key source",
          "[g]enerate / [p]aste nsec / [c]ommand",
        )) || "g";
        let privateKey: PrivateKeySource;
        let ourNpub: string;
        if (choice.startsWith("c")) {
          const cmd = (await ctx.ui.input(
            "Key command",
            "e.g. rbw get nostr-pi  |  pass show nostr/pi",
          )) || "";
          if (!cmd) return;
          privateKey = { type: "command", value: cmd };
          try {
            const k = deriveKeys(resolvePrivateKey(privateKey), "0".repeat(64));
            ourNpub = npubEncode(k.pk);
          } catch (e) {
            ctx.ui.notify(
              `Command failed or bad key: ${(e as Error).message}`,
              "error",
            );
            return;
          }
        } else if (choice.startsWith("p")) {
          const v =
            (await ctx.ui.input("pi private key", "nsec1… or 64-char hex")) ||
            "";
          if (!v) return;
          privateKey = { type: "literal", value: v };
          try {
            const k = deriveKeys(v, "0".repeat(64));
            ourNpub = npubEncode(k.pk);
          } catch (e) {
            ctx.ui.notify(`Invalid key: ${(e as Error).message}`, "error");
            return;
          }
        } else {
          const kp = generateKeypair();
          privateKey = { type: "literal", value: kp.sk };
          ourNpub = kp.npub;
          ctx.ui.notify(
            `Generated key. Add this npub as a contact:\n${kp.npub}`,
            "info",
          );
        }

        const recipient = (await ctx.ui.input(
          "Your npub",
          "npub1… or hex — DMs from this key control pi",
        )) || "";
        if (!recipient) return;
        try {
          parsePubkey(recipient);
        } catch (e) {
          ctx.ui.notify(`Invalid npub: ${(e as Error).message}`, "error");
          return;
        }
        const relayStr = (await ctx.ui.input(
          "Relays (comma-separated)",
          "wss://relay.damus.io,wss://nos.lol,wss://relay.primal.net",
        )) || "wss://relay.damus.io,wss://nos.lol,wss://relay.primal.net";
        const relays = relayStr
          .split(",")
          .map((r) => r.trim())
          .filter(Boolean);
        const blossomStr = (await ctx.ui.input(
          "Blossom servers (comma-separated, optional)",
          "https://blossom.primal.net",
        )) || "";
        const blossomServers = blossomStr
          .split(",")
          .map((r) => r.trim())
          .filter(Boolean);

        config = {
          privateKey,
          recipientPubkey: recipient,
          relays,
          blossomServers,
          enabled: true,
        };
        const path = saveConfig(config);
        ctx.ui.notify(`Saved ${path}\npi npub: ${ourNpub}`, "info");

        bridge?.stop();
        startBridge(ctx);
        bridge?.sendDM("👋 nostr-walkie configured");
        return;
      }

      if (sub === "on" || sub === "off") {
        if (!config) {
          ctx.ui.notify("nostr-walkie: run setup first", "error");
          return;
        }
        config.enabled = sub === "on";
        saveConfig(config);
        if (config.enabled) {
          bridge?.stop();
          startBridge(ctx);
          ctx.ui.notify("nostr-walkie enabled", "info");
        } else {
          bridge?.stop();
          bridge = undefined;
          if (ctx.hasUI) ctx.ui.setStatus("nostr-walkie", "");
          ctx.ui.notify("nostr-walkie disabled", "info");
        }
        return;
      }

      // default: show status
      if (!config) {
        ctx.ui.notify(
          "nostr-walkie: not configured\nRun: /nostr-walkie setup",
          "info",
        );
        return;
      }
      const state = config.enabled
        ? bridge ? "connected" : "enabled (not connected)"
        : "disabled";
      const npub = bridge?.npub() ?? "—";
      ctx.ui.notify(
        `nostr-walkie: ${state}\n` +
          `pi npub: ${npub}\n` +
          `relays: ${config.relays.join(", ")}\n` +
          `blossom: ${config.blossomServers.join(", ") || "(none)"}\n` +
          `Commands: /nostr-walkie setup | on | off`,
        "info",
      );
    },
  });
}

/**
 * Pull the last assistant text block out of the agent_end messages array.
 * Walks backwards, concatenates text parts from the newest assistant msg.
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
function extractLastAssistantText(messages: any[]): string {
  for (let i = messages.length - 1; i >= 0; i--) {
    const m = messages[i];
    if (m?.role !== "assistant") continue;
    const parts = Array.isArray(m.content) ? m.content : [];
    const text = parts
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      .filter((p: any) => p?.type === "text" && typeof p.text === "string")
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      .map((p: any) => p.text)
      .join("");
    if (text.trim()) return text;
  }
  return "";
}

const FILE_TAG_RE = /<nostr-file>([^<]+)<\/nostr-file>\s*/g;

/** Extract and strip <nostr-file>path</nostr-file> tags from assistant text. */
function stripFileTags(text: string): { text: string; files: string[] } {
  const files: string[] = [];
  const stripped = text.replace(FILE_TAG_RE, (_m, p) => {
    files.push(String(p).trim());
    return "";
  });
  return { text: stripped.trim() || "(file attached)", files };
}

/** Build a pi user-message payload for an inbound file. Images are
 *  inlined as base64 attachments; other types are referenced by path. */
function buildFilePrompt(
  path: string,
  mimeType: string,
): string | Array<{ type: string; [k: string]: unknown }> {
  if (mimeType.startsWith("image/")) {
    try {
      const data = readFileSync(path).toString("base64");
      return [
        { type: "text", text: `(image received via Nostr, saved to ${path})` },
        {
          type: "image",
          source: { type: "base64", mediaType: mimeType, data },
        },
      ];
    } catch {
      /* fall through to path ref */
    }
  }
  return `(file received via Nostr: ${path}, ${mimeType})`;
}
