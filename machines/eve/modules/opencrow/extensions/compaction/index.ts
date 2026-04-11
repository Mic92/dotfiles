/**
 * Compaction Extension — personal-assistant summaries for opencrow
 *
 * Why: pi's default compaction is tuned for a coding agent. Its cut-point
 * heuristic (chars/4 vs keepRecentTokens=20k) frequently selects the very
 * first entry when image-heavy chat sessions are involved, which yields an
 * empty messagesToSummarize and a "(none)" summary. The structured
 * Goal/Progress/Blocked template also doesn't fit a personal assistant.
 *
 * This hook replaces the default behaviour:
 *   - summarizes the *entire* span since the previous compaction, ignoring
 *     pi's cut-point heuristic
 *   - keeps only a small tail of recent raw messages
 *   - uses a prompt oriented around user facts / open tasks instead of a
 *     code-project checklist
 *
 * The resulting summary is also what the memory extension stores in
 * sediment via session_compact, so quality here directly improves recall.
 */

import type { ExtensionAPI, SessionEntry } from "@mariozechner/pi-coding-agent";
import {
  convertToLlm,
  serializeConversation,
} from "@mariozechner/pi-coding-agent";
import { complete } from "@mariozechner/pi-ai";

// How many trailing raw entries (messages, not headers) to keep verbatim
// after the summary. Kept small because the summary itself is meant to be
// self-sufficient and the morpheus models have a 64k window.
const KEEP_TAIL_ENTRIES = 6;

const PROMPT = `You are compacting the chat history of a personal assistant.
Produce a concise markdown digest (target: 200-400 words) that lets the
assistant continue the conversation naturally. Base it ONLY on the
conversation below; do not invent facts.

Cover, omitting sections that would be empty:

### About the user
Stable facts, preferences, names, locations, accounts, recurring habits.

### Recent activity
What the assistant did for the user recently (1 line each).

### Open items
Anything the user asked for that is not finished, or that the assistant
promised to follow up on.

### Notes
Anything else needed to keep the conversation coherent (e.g. files
mentioned, IDs, dates).`;

export default function (pi: ExtensionAPI) {
  pi.on("session_before_compact", async (event, ctx) => {
    const { preparation, branchEntries, signal } = event;

    // Walk the actual branch ourselves: from the previous compaction's
    // firstKeptEntryId (or start) up to the end. This sidesteps pi's
    // cut-point heuristic which can produce an empty messagesToSummarize.
    let start = 0;
    for (let i = branchEntries.length - 1; i >= 0; i--) {
      const e = branchEntries[i] as SessionEntry & {
        firstKeptEntryId?: string;
      };
      if (e.type === "compaction") {
        const kept = e.firstKeptEntryId;
        const idx = kept
          ? branchEntries.findIndex((x) => (x as SessionEntry).id === kept)
          : -1;
        start = idx >= 0 ? idx : i + 1;
        break;
      }
    }

    // Decide what to keep verbatim: the last few message entries.
    const msgEntries = branchEntries
      .slice(start)
      .filter((e) => e.type === "message");
    const tailStart = Math.max(0, msgEntries.length - KEEP_TAIL_ENTRIES);

    // Ensure the kept tail begins at a user message so the LLM doesn't see
    // a dangling tool result first.
    let tailIdx = tailStart;
    while (
      tailIdx < msgEntries.length &&
      (msgEntries[tailIdx] as { message: { role: string } }).message.role !==
        "user"
    ) {
      tailIdx++;
    }
    if (tailIdx >= msgEntries.length) tailIdx = msgEntries.length; // keep nothing raw

    const firstKept = msgEntries[tailIdx] as SessionEntry | undefined;
    const firstKeptEntryId = firstKept?.id ?? preparation.firstKeptEntryId;

    const toSummarize = msgEntries
      .slice(0, tailIdx)
      .map((e) => (e as { message: unknown }).message);

    if (toSummarize.length === 0) {
      // Nothing meaningful to compact; cancel so pi doesn't write a (none)
      // entry that would poison future compactions.
      return { cancel: true };
    }

    const conversationText = serializeConversation(
      convertToLlm(toSummarize as never),
    );
    const prev = preparation.previousSummary
      ? `\n\n<previous-summary>\n${preparation.previousSummary}\n</previous-summary>`
      : "";

    const model = ctx.model;
    if (!model) return;
    const auth = await ctx.modelRegistry.getApiKeyAndHeaders(model);
    if (!auth.ok || !auth.apiKey) {
      ctx.ui.notify(
        `compaction: auth failed (${
          "error" in auth ? auth.error : "no key"
        }), falling back to default`,
        "warning",
      );
      return;
    }

    let summary: string;
    try {
      const resp = await complete(
        model,
        {
          messages: [
            {
              role: "user",
              content: [
                {
                  type: "text",
                  text:
                    `${PROMPT}${prev}\n\n<conversation>\n${conversationText}\n</conversation>`,
                },
              ],
              timestamp: Date.now(),
            },
          ],
        },
        { apiKey: auth.apiKey, headers: auth.headers, maxTokens: 2048, signal },
      );
      summary = resp.content
        .filter((c): c is { type: "text"; text: string } => c.type === "text")
        .map((c) => c.text)
        .join("\n")
        .trim();
    } catch (e) {
      ctx.ui.notify(
        `compaction: ${e instanceof Error ? e.message : String(e)}`,
        "error",
      );
      return; // fall back to default
    }

    if (!summary) return { cancel: true };

    return {
      compaction: {
        summary,
        firstKeptEntryId,
        tokensBefore: preparation.tokensBefore,
      },
    };
  });
}
