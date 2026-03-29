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
  SessionSwitchEvent,
} from "@mariozechner/pi-coding-agent";
import { compact, DynamicBorder } from "@mariozechner/pi-coding-agent";
import {
  Container,
  type SelectItem,
  SelectList,
  Text,
} from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";

type LoopMode = "tests" | "custom" | "self";

interface LoopState {
  active: boolean;
  mode?: LoopMode;
  condition?: string;
  prompt?: string;
  summary?: string;
  loopCount?: number;
}

const PRESETS = [
  { value: "tests", label: "Until tests pass", description: "" },
  { value: "custom", label: "Until custom condition", description: "" },
  { value: "self", label: "Self driven (agent decides)", description: "" },
] as const;

const STATE_ENTRY = "until-state";
const HAIKU_ID = "claude-haiku-4-5";

const SUMMARY_SYSTEM_PROMPT =
  `You summarize loop breakout conditions for a status widget.
Return a concise phrase (max 6 words) that says when the loop should stop.
Use plain text only, no quotes, no punctuation, no prefix.

Form should be "breaks when ...", "loops until ...", "stops on ...", "runs until ...", or similar.
Use the best form that makes sense for the loop condition.`;

function buildPrompt(mode: LoopMode, condition?: string): string {
  switch (mode) {
    case "tests":
      return (
        "Run all tests. If they are passing, call the signal_loop_success tool. " +
        "Otherwise continue until the tests pass."
      );
    case "custom": {
      const c = condition?.trim() || "the custom condition is satisfied";
      return (
        `Continue until the following condition is satisfied: ${c}. ` +
        "When it is satisfied, call the signal_loop_success tool."
      );
    }
    case "self":
      return "Continue until you are done. When finished, call the signal_loop_success tool.";
  }
}

function fallbackSummary(mode: LoopMode, condition?: string): string {
  switch (mode) {
    case "tests":
      return "tests pass";
    case "custom": {
      const s = condition?.trim() || "custom condition";
      return s.length > 48 ? `${s.slice(0, 45)}...` : s;
    }
    case "self":
      return "done";
  }
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

async function pickSummaryModel(
  ctx: ExtensionContext,
): Promise<{ model: Model<Api>; apiKey: string } | null> {
  if (!ctx.model) return null;
  // Prefer cheap Haiku when the main provider is Anthropic.
  if (ctx.model.provider === "anthropic") {
    const haiku = ctx.modelRegistry.find("anthropic", HAIKU_ID);
    if (haiku) {
      const auth = await ctx.modelRegistry.getApiKeyAndHeaders(haiku);
      if (auth.ok && auth.apiKey) return { model: haiku, apiKey: auth.apiKey };
    }
  }
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
  const fallback = fallbackSummary(mode, condition);
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
  if (!s) return fallback;
  return s.length > 60 ? `${s.slice(0, 57)}...` : s;
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

async function loadState(ctx: ExtensionContext): Promise<LoopState> {
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

  const persist = (s: LoopState) => pi.appendEntry(STATE_ENTRY, s);

  function set(s: LoopState, ctx: ExtensionContext) {
    state = s;
    persist(s);
    updateWidget(ctx, s);
  }

  function clear(ctx: ExtensionContext) {
    set({ active: false }, ctx);
  }

  function lastAssistantAborted(
    messages: Array<{ role?: string; stopReason?: string }>,
  ): boolean {
    for (let i = messages.length - 1; i >= 0; i--) {
      if (messages[i]?.role === "assistant") {
        return messages[i].stopReason === "aborted";
      }
    }
    return false;
  }

  function fire(ctx: ExtensionContext) {
    if (!state.active || !state.prompt) return;
    if (ctx.hasPendingMessages()) return;
    state = { ...state, loopCount: (state.loopCount ?? 0) + 1 };
    persist(state);
    updateWidget(ctx, state);
    pi.sendMessage(
      { customType: "until", content: state.prompt, display: true },
      { deliverAs: "followUp", triggerTurn: true },
    );
  }

  async function picker(ctx: ExtensionContext): Promise<LoopState | null> {
    const items: SelectItem[] = PRESETS.map((p) => ({
      value: p.value,
      label: p.label,
      description: p.description,
    }));

    const sel = await ctx.ui.custom<string | null>((tui, theme, _kb, done) => {
      const c = new Container();
      c.addChild(new DynamicBorder((s) => theme.fg("accent", s)));
      c.addChild(
        new Text(theme.fg("accent", theme.bold("Select loop preset"))),
      );
      const list = new SelectList(items, Math.min(items.length, 10), {
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
    if (sel === "tests") {
      return { active: true, mode: "tests", prompt: buildPrompt("tests") };
    }
    if (sel === "self") {
      return { active: true, mode: "self", prompt: buildPrompt("self") };
    }
    const cond = await ctx.ui.editor("Enter loop breakout condition:", "");
    if (!cond?.trim()) return null;
    return {
      active: true,
      mode: "custom",
      condition: cond.trim(),
      prompt: buildPrompt("custom", cond.trim()),
    };
  }

  function parse(args: string | undefined): LoopState | null {
    if (!args?.trim()) return null;
    const parts = args.trim().split(/\s+/);
    const mode = parts[0]?.toLowerCase();
    if (mode === "tests") {
      return { active: true, mode: "tests", prompt: buildPrompt("tests") };
    }
    if (mode === "self") {
      return { active: true, mode: "self", prompt: buildPrompt("self") };
    }
    if (mode === "custom") {
      const c = parts.slice(1).join(" ").trim();
      if (!c) return null;
      return {
        active: true,
        mode: "custom",
        condition: c,
        prompt: buildPrompt("custom", c),
      };
    }
    return null;
  }

  pi.registerTool({
    name: "signal_loop_success",
    label: "Signal Loop Success",
    description:
      "Stop the active /until loop when the breakout condition is satisfied. " +
      "Only call this tool when explicitly instructed to do so.",
    parameters: Type.Object({}),
    async execute(_id, _p, _sig, _upd, ctx) {
      if (!state.active) {
        return {
          content: [{ type: "text", text: "No active loop is running." }],
          details: { active: false },
        };
      }
      clear(ctx);
      return {
        content: [{ type: "text", text: "Loop ended." }],
        details: { active: false },
      };
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

      // Async: get a short summary for the status widget.
      const { mode, condition } = next;
      void (async () => {
        const s = await summarize(ctx, mode!, condition);
        if (
          !state.active || state.mode !== mode || state.condition !== condition
        ) {
          return;
        }
        state = { ...state, summary: s };
        persist(state);
        updateWidget(ctx, state);
      })();
    },
  });

  pi.on("agent_end", async (ev, ctx) => {
    if (!state.active) return;
    if (ctx.hasUI && lastAssistantAborted(ev.messages)) {
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
    fire(ctx);
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
      }. Preserve this loop state and breakout condition in the summary.`,
    ]
      .filter(Boolean)
      .join("\n\n");
    try {
      const compaction = await compact(
        ev.preparation,
        ctx.model,
        auth.apiKey,
        auth.headers,
        instr,
        ev.signal,
      );
      return { compaction };
    } catch (e) {
      if (ctx.hasUI) {
        ctx.ui.notify(
          `Loop compaction failed: ${(e as Error).message}`,
          "warning",
        );
      }
    }
  });

  async function restore(ctx: ExtensionContext) {
    state = await loadState(ctx);
    updateWidget(ctx, state);
    if (state.active && state.mode && !state.summary) {
      const { mode, condition } = state;
      void (async () => {
        const s = await summarize(ctx, mode, condition);
        if (
          !state.active || state.mode !== mode || state.condition !== condition
        ) {
          return;
        }
        state = { ...state, summary: s };
        persist(state);
        updateWidget(ctx, state);
      })();
    }
  }

  pi.on("session_start", async (_ev, ctx) => restore(ctx));
  pi.on("session_switch", async (_ev: SessionSwitchEvent, ctx) => restore(ctx));
}
