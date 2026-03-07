/**
 * Memory Extension — cross-session recall for opencrow
 *
 * Captures conversation content into sediment (a local semantic vector
 * store) so that knowledge survives session restarts. On each new
 * prompt, relevant memories are recalled and injected into context.
 *
 * Flow:
 *   session_compact    → store the compaction summary in sediment
 *   agent_end          → store each turn's conversation
 *   before_agent_start → recall relevant memories, inject into context
 *   memory_search tool → let the LLM search explicitly
 */

import {
  convertToLlm,
  serializeConversation,
} from "@mariozechner/pi-coding-agent";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";

const MIN_SIMILARITY = 0.45;
const AUTO_RECALL_LIMIT = 3;
const SEDIMENT_TIMEOUT = 10_000;

// ── sediment helpers ─────────────────────────────────────────────────

/** Track whether sediment is available to avoid repeated timeouts. */
let sedimentAvailable: boolean | undefined;

async function sediment(
  pi: ExtensionAPI,
  args: string[],
  signal?: AbortSignal,
): Promise<string> {
  if (sedimentAvailable === false) {
    throw new Error("sediment unavailable");
  }

  const result = await pi.exec("sediment", args, {
    signal,
    timeout: SEDIMENT_TIMEOUT,
  });

  if (result.code !== 0) {
    // Detect missing binary: killed by timeout or command-not-found exit codes
    if (result.killed || result.code === 127) {
      sedimentAvailable = false;
    }
    throw new Error(`sediment ${args[0]} failed: ${result.stderr}`);
  }

  sedimentAvailable = true;
  return result.stdout;
}

interface RecallResult {
  content: string;
  id: string;
  similarity: string;
}

async function recall(
  pi: ExtensionAPI,
  query: string,
  limit: number,
  signal?: AbortSignal,
): Promise<RecallResult[]> {
  const raw = await sediment(
    pi,
    ["recall", query, "--limit", String(limit), "--json"],
    signal,
  );
  const parsed = JSON.parse(raw) as { results: RecallResult[] };
  return parsed.results.filter(
    (r) => parseFloat(r.similarity) >= MIN_SIMILARITY,
  );
}

// ── extension ────────────────────────────────────────────────────────

export default function (pi: ExtensionAPI) {
  // ── Capture: store compaction summaries ──────────────────────────

  pi.on("session_compact", async (event) => {
    const summary = event.compactionEntry.summary;
    if (!summary?.trim()) return;

    try {
      await sediment(pi, ["store", summary]);
    } catch (e) {
      console.error("memory: failed to store compaction summary", e);
    }
  });

  // ── Capture: store each turn's conversation ──────────────────────
  // Each agent_end contains only the messages from that prompt cycle,
  // so each turn is stored as a separate entry. Sediment deduplicates
  // near-identical content automatically.

  pi.on("agent_end", async (event) => {
    const messages = event.messages;
    if (messages.length < 2) return;

    const text = serializeConversation(convertToLlm(messages));
    if (text.trim().length < 100) return;

    const truncated = text.length > 4000 ? text.slice(0, 4000) + "\n..." : text;

    try {
      await sediment(pi, ["store", truncated]);
    } catch (e) {
      console.error("memory: failed to store conversation", e);
    }
  });

  // ── Recall: inject relevant memories before each prompt ─────────

  pi.on("before_agent_start", async (event) => {
    const prompt = event.prompt;
    if (!prompt?.trim()) return;

    try {
      const results = await recall(pi, prompt, AUTO_RECALL_LIMIT);
      if (results.length === 0) return;

      const block = results.map((r) => r.content).join("\n\n---\n\n");

      return {
        systemPrompt: event.systemPrompt +
          "\n\n<recalled_memories>\n" +
          "The following are relevant memories from previous conversations. " +
          "Use them to provide continuity but do not mention them unless asked.\n\n" +
          block +
          "\n</recalled_memories>",
      };
    } catch {
      // sediment unavailable — proceed without memories
    }
  });

  // ── Explicit tool: let the LLM search on demand ─────────────────

  pi.registerTool({
    name: "memory_search",
    label: "Memory Search",
    description: "Semantic search across memories from past conversations.",
    promptGuidelines: [
      "Search memory when asked about past conversations or user preferences.",
    ],
    parameters: Type.Object({
      query: Type.String({ description: "Search query" }),
      limit: Type.Optional(
        Type.Number({ description: "Max results (default 5)", default: 5 }),
      ),
    }),

    async execute(_toolCallId, params, signal) {
      try {
        const raw = await sediment(
          pi,
          [
            "recall",
            params.query,
            "--limit",
            String(params.limit ?? 5),
            "--json",
          ],
          signal,
        );
        const parsed = JSON.parse(raw) as { results: RecallResult[] };
        const results = parsed.results;

        if (results.length === 0) {
          return {
            content: [{ type: "text", text: "No memories found." }],
            details: { results: [] },
          };
        }

        const text = results
          .map((r) => `[similarity=${r.similarity}]\n${r.content}`)
          .join("\n\n---\n\n");

        return {
          content: [{ type: "text", text }],
          details: { results },
        };
      } catch (e) {
        const msg = e instanceof Error ? e.message : String(e);
        return {
          content: [{ type: "text", text: `Memory search failed: ${msg}` }],
          details: { error: msg },
        };
      }
    },
  });
}
