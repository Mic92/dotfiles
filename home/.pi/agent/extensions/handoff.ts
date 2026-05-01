/**
 * Handoff — transfer context to a new focused session.
 *
 * Instead of /compact (which blindly summarizes everything), /handoff
 * asks the current model to extract only what matters for your *next*
 * goal and generates a self-contained prompt. You review it in $EDITOR,
 * then it spawns a new session (parent-linked) with the prompt pre-filled.
 *
 * Usage:
 *   /handoff now implement this for teams as well
 *   /handoff execute phase one of the plan
 *   /handoff check other places that need this fix
 *
 * Based on mariozechner's examples/extensions/handoff.ts,
 * via aldoborrero/agent-kit.
 */
import type { AgentMessage } from "@mariozechner/pi-agent-core";
import { complete, type Message } from "@mariozechner/pi-ai";
import type { ExtensionAPI, SessionEntry } from "@mariozechner/pi-coding-agent";
import {
  BorderedLoader,
  convertToLlm,
  serializeConversation,
} from "@mariozechner/pi-coding-agent";

const SYSTEM_PROMPT =
  `You are a context transfer assistant. Given a conversation history and the user's goal for a new thread, generate a focused prompt that:

1. Summarizes relevant context from the conversation (decisions made, approaches taken, key findings)
2. Lists any relevant files that were discussed or modified
3. Clearly states the next task based on the user's goal
4. Is self-contained - the new thread should be able to proceed without the old conversation

Format your response as a prompt the user can send to start the new thread. Be concise but include all necessary context. Do not include any preamble like "Here's the prompt" - just output the prompt itself.

Example output format:
## Context
We've been working on X. Key decisions:
- Decision 1
- Decision 2

Files involved:
- path/to/file1.ts
- path/to/file2.ts

## Task
[Clear description of what to do next based on user's goal]`;

function entryToMessage(entry: SessionEntry): AgentMessage | undefined {
  if (entry.type === "message") {
    return entry.message;
  }
  if (entry.type === "compaction") {
    return {
      role: "compactionSummary",
      summary: entry.summary,
      tokensBefore: entry.tokensBefore,
      timestamp: new Date(entry.timestamp).getTime(),
    };
  }
  return undefined;
}

// If the branch was compacted, include the compaction summary plus the
// kept tail so the handoff model sees the same context the agent would.
function getHandoffMessages(branch: SessionEntry[]): AgentMessage[] {
  let compactionIndex = -1;
  for (let i = branch.length - 1; i >= 0; i--) {
    if (branch[i].type === "compaction") {
      compactionIndex = i;
      break;
    }
  }
  if (compactionIndex < 0) {
    return branch.map(entryToMessage).filter((m) => m !== undefined);
  }

  const compaction = branch[compactionIndex];
  const firstKeptIndex = compaction.type === "compaction"
    ? branch.findIndex((e) => e.id === compaction.firstKeptEntryId)
    : -1;
  const compactedBranch = [
    compaction,
    ...(firstKeptIndex >= 0
      ? branch.slice(firstKeptIndex, compactionIndex)
      : []),
    ...branch.slice(compactionIndex + 1),
  ];
  return compactedBranch.map(entryToMessage).filter((m) => m !== undefined);
}

export default function (pi: ExtensionAPI) {
  pi.registerCommand("handoff", {
    description: "Transfer context to a new focused session",
    handler: async (args, ctx) => {
      if (!ctx.hasUI) {
        ctx.ui.notify("handoff requires interactive mode", "error");
        return;
      }
      if (!ctx.model) {
        ctx.ui.notify("No model selected", "error");
        return;
      }

      const goal = args.trim();
      if (!goal) {
        ctx.ui.notify("Usage: /handoff <goal for new thread>", "error");
        return;
      }

      const messages = getHandoffMessages(ctx.sessionManager.getBranch());

      if (messages.length === 0) {
        ctx.ui.notify("No conversation to hand off", "error");
        return;
      }

      const llmMessages = convertToLlm(messages);
      const conversationText = serializeConversation(llmMessages);
      const currentSessionFile = ctx.sessionManager.getSessionFile();

      // One-shot LLM call with a cancellable loader.
      const result = await ctx.ui.custom<string | null>(
        (tui, theme, _kb, done) => {
          const loader = new BorderedLoader(
            tui,
            theme,
            "Generating handoff prompt...",
          );
          loader.onAbort = () => done(null);

          const generate = async () => {
            const auth = await ctx.modelRegistry.getApiKeyAndHeaders(
              ctx.model!,
            );
            if (!auth.ok || !auth.apiKey) {
              throw new Error(
                auth.ok ? `No API key for ${ctx.model!.provider}` : auth.error,
              );
            }
            const userMessage: Message = {
              role: "user",
              content: [
                {
                  type: "text",
                  text:
                    `## Conversation History\n\n${conversationText}\n\n## User's Goal for New Thread\n\n${goal}`,
                },
              ],
              timestamp: Date.now(),
            };

            const response = await complete(
              ctx.model!,
              { systemPrompt: SYSTEM_PROMPT, messages: [userMessage] },
              {
                apiKey: auth.apiKey,
                headers: auth.headers,
                signal: loader.signal,
              },
            );

            if (response.stopReason === "aborted") return null;

            return response.content
              .filter(
                (c): c is { type: "text"; text: string } => c.type === "text",
              )
              .map((c) => c.text)
              .join("\n");
          };

          generate()
            .then(done)
            .catch((err) => {
              // eslint-disable-next-line no-console
              console.error("handoff: generation failed:", err);
              done(null);
            });

          return loader;
        },
      );

      if (result === null) {
        ctx.ui.notify("Cancelled", "info");
        return;
      }

      const edited = await ctx.ui.editor("Edit handoff prompt", result);
      if (edited === undefined) {
        ctx.ui.notify("Cancelled", "info");
        return;
      }

      // After a successful session replacement the original ctx is stale;
      // use the replacement ctx for post-switch UI work.
      const newSession = await ctx.newSession({
        parentSession: currentSessionFile,
        withSession: async (replacementCtx) => {
          replacementCtx.ui.setEditorText(edited);
          replacementCtx.ui.notify("Handoff ready. Submit when ready.", "info");
        },
      });
      if (newSession.cancelled) {
        ctx.ui.notify("New session cancelled", "info");
      }
    },
  });
}
