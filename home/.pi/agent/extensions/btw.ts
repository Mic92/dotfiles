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
  pi.on("context", async (event) => ({
    messages: event.messages.filter(
      (m) => (m as { customType?: string }).customType !== "btw",
    ),
  }));
}
