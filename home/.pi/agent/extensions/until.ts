/**
 * /until — retry-until-success loop for TDD and iterative workflows.
 *
 * Keeps re-prompting the agent after each agent_end until it calls the
 * signal_loop_success tool. State persists across /resume via custom
 * session entries, and compaction is hooked so the breakout condition
 * survives context squashing.
 *
 * Usage:
 *   /until               → interactive picker (tests / custom / self)
 *   /until tests         → re-run tests until green
 *   /until custom <cond> → until user-defined condition
 *   /until self          → agent decides when done
 *
 * Ported from aldoborrero/agent-kit (extensions/until).
 */
import {
  type Api,
  complete,
  type Model,
  type UserMessage,
} from "@mariozechner/pi-ai";
import type {
  ExtensionAPI,
  ExtensionContext,
} from "@mariozechner/pi-coding-agent";
import { compact, DynamicBorder } from "@mariozechner/pi-coding-agent";
import {
  Container,
  type SelectItem,
  SelectList,
  Text,
} from "@mariozechner/pi-tui";
import { Type } from "typebox";

type LoopMode = "tests" | "custom" | "self";

interface LoopState {
  active: boolean;
  mode?: LoopMode;
  condition?: string;
  prompt?: string;
  summary?: string;
  loopCount?: number;
}

const PRESETS: SelectItem[] = [
  { value: "tests", label: "Until tests pass" },
  { value: "custom", label: "Until custom condition" },
  { value: "self", label: "Self driven (agent decides)" },
];

const STATE_ENTRY = "until-state";

// Cheap models tried in order for the widget summary; first one with working
// auth wins. Local Qwen on morpheus is free, Haiku is the paid fallback.
const SUMMARY_MODELS: Array<[provider: string, id: string]> = [
  ["morpheus", "qwen-35-35b-coding"],
  ["anthropic", "claude-haiku-4-5"],
];

const SUMMARY_SYSTEM_PROMPT =
  `You summarize loop breakout conditions for a status widget.
Return a concise phrase (max 6 words) that says when the loop should stop.
Use plain text only, no quotes, no punctuation, no prefix.

Form should be "breaks when ...", "loops until ...", "stops on ...", "runs until ...", or similar.
Use the best form that makes sense for the loop condition.`;

function truncate(s: string, n: number): string {
  return s.length > n ? `${s.slice(0, n - 3)}...` : s;
}

function conditionText(mode: LoopMode, condition?: string): string {
  switch (mode) {
    case "tests":
      return "tests pass";
    case "custom":
      return condition?.trim() || "custom condition";
    case "self":
      return "you are done";
  }
}

function buildPrompt(mode: LoopMode, condition?: string): string {
  switch (mode) {
    case "tests":
      return (
        "Run all tests. If they are passing, call the signal_loop_success tool. " +
        "Otherwise continue until the tests pass."
      );
    case "custom":
      return (
        `Continue until the following condition is satisfied: ${
          conditionText(mode, condition)
        }. ` +
        "When it is satisfied, call the signal_loop_success tool."
      );
    case "self":
      return "Continue until you are done. When finished, call the signal_loop_success tool.";
  }
}

function makeState(mode: LoopMode, condition?: string): LoopState {
  return {
    active: true,
    mode,
    condition,
    prompt: buildPrompt(mode, condition),
  };
}

async function pickSummaryModel(
  ctx: ExtensionContext,
): Promise<{ model: Model<Api>; apiKey: string } | null> {
  for (const [provider, id] of SUMMARY_MODELS) {
    const m = ctx.modelRegistry.find(provider, id);
    if (!m) continue;
    const auth = await ctx.modelRegistry.getApiKeyAndHeaders(m);
    if (auth.ok && auth.apiKey) return { model: m, apiKey: auth.apiKey };
  }
  if (!ctx.model) return null;
  const auth = await ctx.modelRegistry.getApiKeyAndHeaders(ctx.model);
  return auth.ok && auth.apiKey
    ? { model: ctx.model, apiKey: auth.apiKey }
    : null;
}

async function summarize(
  ctx: ExtensionContext,
  mode: LoopMode,
  condition?: string,
): Promise<string> {
  const fallback = truncate(conditionText(mode, condition), 48);
  const sel = await pickSummaryModel(ctx);
  if (!sel) return fallback;

  const msg: UserMessage = {
    role: "user",
    content: [{ type: "text", text: conditionText(mode, condition) }],
    timestamp: Date.now(),
  };
  const r = await complete(
    sel.model,
    { systemPrompt: SUMMARY_SYSTEM_PROMPT, messages: [msg] },
    { apiKey: sel.apiKey },
  );
  if (r.stopReason === "aborted" || r.stopReason === "error") return fallback;

  const s = r.content
    .filter((c): c is { type: "text"; text: string } => c.type === "text")
    .map((c) => c.text)
    .join(" ")
    .replace(/\s+/g, " ")
    .trim();
  return s ? truncate(s, 60) : fallback;
}

function updateWidget(ctx: ExtensionContext, state: LoopState): void {
  if (!ctx.hasUI) return;
  if (!state.active || !state.mode) {
    ctx.ui.setWidget("until", undefined);
    return;
  }
  const n = state.loopCount ?? 0;
  const txt = state.summary
    ? `Until: ${state.summary} (turn ${n})`
    : `Until active (turn ${n})`;
  ctx.ui.setWidget("until", [ctx.ui.theme.fg("accent", txt)]);
}

function loadState(ctx: ExtensionContext): LoopState {
  const entries = ctx.sessionManager.getEntries();
  for (let i = entries.length - 1; i >= 0; i--) {
    const e = entries[i] as {
      type: string;
      customType?: string;
      data?: LoopState;
    };
    if (e.type === "custom" && e.customType === STATE_ENTRY && e.data) {
      return e.data;
    }
  }
  return { active: false };
}

export default function (pi: ExtensionAPI): void {
  let state: LoopState = { active: false };

  function set(s: LoopState, ctx: ExtensionContext) {
    state = s;
    pi.appendEntry(STATE_ENTRY, s);
    updateWidget(ctx, s);
  }

  const clear = (ctx: ExtensionContext) => set({ active: false }, ctx);

  function fire(ctx: ExtensionContext) {
    if (!state.active || !state.prompt) return;
    if (ctx.hasPendingMessages()) return;
    set({ ...state, loopCount: (state.loopCount ?? 0) + 1 }, ctx);
    pi.sendMessage(
      { customType: "until", content: state.prompt, display: true },
      { deliverAs: "followUp", triggerTurn: true },
    );
  }

  // Fetch a short widget label out-of-band; ignore the result if the loop
  // it was requested for has been replaced or stopped meanwhile.
  function refreshSummary(
    ctx: ExtensionContext,
    mode: LoopMode,
    condition?: string,
  ) {
    void summarize(ctx, mode, condition).then((s) => {
      if (
        !state.active || state.mode !== mode || state.condition !== condition
      ) return;
      set({ ...state, summary: s }, ctx);
    });
  }

  async function picker(ctx: ExtensionContext): Promise<LoopState | null> {
    const sel = await ctx.ui.custom<string | null>((tui, theme, _kb, done) => {
      const c = new Container();
      c.addChild(new DynamicBorder((s) => theme.fg("accent", s)));
      c.addChild(
        new Text(theme.fg("accent", theme.bold("Select loop preset"))),
      );
      const list = new SelectList(PRESETS, PRESETS.length, {
        selectedPrefix: (t) => theme.fg("accent", t),
        selectedText: (t) => theme.fg("accent", t),
        description: (t) => theme.fg("muted", t),
        scrollInfo: (t) => theme.fg("dim", t),
        noMatch: (t) => theme.fg("warning", t),
      });
      list.onSelect = (i) => done(i.value);
      list.onCancel = () => done(null);
      c.addChild(list);
      c.addChild(new Text(theme.fg("dim", "Enter to confirm, Esc to cancel")));
      c.addChild(new DynamicBorder((s) => theme.fg("accent", s)));
      return {
        render: (w: number) => c.render(w),
        invalidate: () => c.invalidate(),
        handleInput: (d: string) => {
          list.handleInput(d);
          tui.requestRender();
        },
      };
    });

    if (!sel) return null;
    if (sel !== "custom") return makeState(sel as LoopMode);
    const cond = (await ctx.ui.editor("Enter loop breakout condition:", ""))
      ?.trim();
    return cond ? makeState("custom", cond) : null;
  }

  function parse(args: string | undefined): LoopState | null {
    const [mode, ...rest] = (args ?? "").trim().split(/\s+/);
    switch (mode?.toLowerCase()) {
      case "tests":
      case "self":
        return makeState(mode as LoopMode);
      case "custom": {
        const c = rest.join(" ").trim();
        return c ? makeState("custom", c) : null;
      }
      default:
        return null;
    }
  }

  pi.registerTool({
    name: "signal_loop_success",
    label: "Signal Loop Success",
    description:
      "Stop the active /until loop when the breakout condition is satisfied. " +
      "Only call this tool when explicitly instructed to do so.",
    parameters: Type.Object({}),
    async execute(_id, _p, _sig, _upd, ctx) {
      const text = state.active ? "Loop ended." : "No active loop is running.";
      if (state.active) clear(ctx);
      return { content: [{ type: "text", text }] };
    },
  });

  pi.registerCommand("until", {
    description:
      "Repeat until a condition is met (/until tests | custom <cond> | self)",
    handler: async (args, ctx) => {
      let next = parse(args);
      if (!next) {
        if (!ctx.hasUI) {
          ctx.ui.notify(
            "Usage: /until tests | /until custom <condition> | /until self",
            "warning",
          );
          return;
        }
        next = await picker(ctx);
      }
      if (!next) {
        ctx.ui.notify("Cancelled", "info");
        return;
      }

      if (state.active) {
        const ok = ctx.hasUI
          ? await ctx.ui.confirm(
            "Replace active loop?",
            "A loop is already active. Replace it?",
          )
          : true;
        if (!ok) return;
      }

      set({ ...next, summary: undefined, loopCount: 0 }, ctx);
      ctx.ui.notify("Loop active", "info");
      fire(ctx);
      refreshSummary(ctx, next.mode!, next.condition);
    },
  });

  pi.on("agent_end", async (ev, ctx) => {
    if (!state.active) return;
    const aborted = ev.messages.findLast((m) =>
      m.role === "assistant"
    )?.stopReason === "aborted";
    if (ctx.hasUI && aborted) {
      const brk = await ctx.ui.confirm(
        "Break loop?",
        "Operation aborted. Break out of the loop?",
      );
      if (brk) {
        clear(ctx);
        ctx.ui.notify("Loop ended", "info");
        return;
      }
    }
    // isStreaming stays true while agent_end listeners run; defer past
    // finishRun() so triggerTurn takes the prompt() path and re-prompts.
    setTimeout(() => fire(ctx), 0);
  });

  // Preserve loop state across compaction so the agent doesn't forget
  // what condition it's working towards.
  pi.on("session_before_compact", async (ev, ctx) => {
    if (!state.active || !state.mode || !ctx.model) return;
    const auth = await ctx.modelRegistry.getApiKeyAndHeaders(ctx.model);
    if (!auth.ok || !auth.apiKey) return;
    const instr = [
      ev.customInstructions,
      `Loop active. Breakout condition: ${
        conditionText(state.mode, state.condition)
      }. ` +
      "Preserve this loop state and breakout condition in the summary.",
    ].filter(Boolean).join("\n\n");
    try {
      return {
        compaction: await compact(
          ev.preparation,
          ctx.model,
          auth.apiKey,
          auth.headers,
          instr,
          ev.signal,
        ),
      };
    } catch (e) {
      if (ctx.hasUI) {
        ctx.ui.notify(
          `Loop compaction failed: ${(e as Error).message}`,
          "warning",
        );
      }
    }
  });

  pi.on("session_start", (_ev, ctx) => {
    state = loadState(ctx);
    updateWidget(ctx, state);
    if (state.active && state.mode && !state.summary) {
      refreshSummary(ctx, state.mode, state.condition);
    }
  });
}
