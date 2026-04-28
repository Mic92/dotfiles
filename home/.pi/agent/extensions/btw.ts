/**
 * BTW — ephemeral side questions that don't pollute context.
 *
 * Ask a quick tangential question mid-task without it accumulating in
 * history. The Q&A is stripped from the LLM's context on the next turn,
 * so it costs tokens once instead of compounding forever.
 *
 * Usage:
 *   /btw what's the difference between rebase and merge?
 *   /btw is argon2 still the recommended password hash?
 *
 * Ported from aldoborrero/agent-kit.
 */
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("btw", {
    description:
      "Ask a quick side question (ephemeral, no tools, doesn't pollute context)",
    handler: async (args, ctx) => {
      const question = args.trim();
      if (!question) {
        ctx.ui.notify("Usage: /btw <question>", "error");
        return;
      }

      pi.sendMessage(
        {
          customType: "btw",
          content:
            `[BTW — side question, answer briefly without using any tools]\n\n${question}`,
          display: true,
        },
        { triggerTurn: true },
      );
    },
  });

  // Strip btw messages from subsequent turns so they don't accumulate.
  // Also strip the assistant reply that immediately follows — otherwise we'd
  // drop the Q but keep the A, leaving a dangling answer in context forever.
  pi.on("context", async (event) => {
    const drop = new Set<number>();
    const msgs = event.messages as Array<{
      customType?: string;
      role?: string;
    }>;
    for (let i = 0; i < msgs.length; i++) {
      if (msgs[i].customType !== "btw") continue;
      // Don't strip the current turn's question — only past Q&As. If the btw
      // message is last, it's the one we're about to answer; removing it would
      // leave the convo ending on an assistant message (API rejects that).
      if (i === msgs.length - 1) continue;
      drop.add(i);
      if (msgs[i + 1]?.role === "assistant") drop.add(i + 1);
    }
    return { messages: event.messages.filter((_, i) => !drop.has(i)) };
  });
}
