import { div, View } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import {
  closeOverlay,
  focused,
  hc,
  nextCommand,
  props,
  run,
  theme,
} from "herdr";

// Mirrors the prefix table of ~/.config/herdr/config.toml (itself a port of
// ~/.tmux.conf), so k closes a pane instead of moving focus; alt-hjkl and
// the arrows move focus.
const REPEAT_MS = 500;
const BINDINGS = [
  ["c", "New tab", "tab.new"],
  ["C", "New space", "space.new"],
  ['"', "Split down", "pane.split", { direction: "down" }],
  ["-", "Split down", "pane.split", { direction: "down" }],
  ["%", "Split right", "pane.split", { direction: "right" }],
  ["v", "Split right", "pane.split", { direction: "right" }],
  ["x", "Close pane", "pane.close"],
  ["k", "Close pane", "pane.close"],
  ["&", "Close tab", "tab.close"],
  ["z", "Zoom pane", "pane.zoom"],
  ["n", "Next tab", "tab.next", {}, true],
  ["p", "Previous tab", "tab.prev", {}, true],
  [")", "Next space", "space.next", {}, true],
  ["(", "Previous space", "space.prev", {}, true],
  ["s", "Picker", "mod.open", { name: "picker" }],
  ["w", "Picker", "mod.open", { name: "picker" }],
  [",", "Settings", "app.settings"],
  ["$", "Rename space", "space.rename"],
  // The closest thing to detach: terminals keep running in herdr-core.
  ["d", "Close window", "window.close"],
  ["left", "Focus left", "pane.go", { direction: "left" }, true],
  ["down", "Focus down", "pane.go", { direction: "down" }, true],
  ["up", "Focus up", "pane.go", { direction: "up" }, true],
  ["right", "Focus right", "pane.go", { direction: "right" }, true],
  ...Array.from(
    { length: 10 },
    (
      _,
      i,
    ) => [String(i), `Tab ${i === 0 ? 10 : i}`, "tab.select", {
      index: i === 0 ? 10 : i,
    }],
  ),
];

function forwardKey(e, key) {
  if (
    ["enter", "tab", "backspace", "left", "right", "up", "down", "space"]
      .includes(e.key)
  ) return e.key;
  if (key === " ") return "space";
  if (/^[A-Z]$/.test(key)) return `shift+${key.toLowerCase()}`;
  if (
    key.length === 1 &&
    "abcdefghijklmnopqrstuvwxyz0123456789`\\[],=-.';/".includes(key)
  ) return key === "\\" ? "backslash" : key;
  return null;
}

export default class TmuxPrefix extends View {
  init(_props, cx) {
    this.handle = cx.focus_handle();
    this.handle.focus();
    this.repeat = false;
    this.generation = 0;
    this.all = false;
    // The mod-specific key binding beats the global opener while we're open.
    cx.spawn(async (cx) => {
      for (;;) {
        await nextCommand();
        this.sendPrefix(cx);
      }
    });
  }

  dismiss() {
    closeOverlay(props().overlay);
  }

  send(key, cx) {
    this.generation++;
    const terminal = focused()?.terminal;
    if (!terminal) return this.dismiss();
    // hc starts its subprocess at the call, before closing cancels this view's tasks.
    const pending = hc(["keys", terminal, key]);
    cx.spawn(async () => {
      const result = await pending;
      if (result.code !== 0) console.log(`tmux-prefix: ${result.stderr}`);
    });
    this.dismiss();
  }

  sendPrefix(cx) {
    this.send("ctrl+space", cx);
  }

  key(e, cx) {
    cx.stop_propagation();
    const m = e.modifiers;
    if (e.key === "escape") return this.dismiss();
    if (m.control && e.key === "space") return this.sendPrefix(cx);
    const plain = !m.control && !m.alt && !m.platform;
    const key = plain
      ? (e.key_char ??
        (m.shift && e.key.length === 1 ? e.key.toUpperCase() : e.key))
      : e.key;
    if (plain && key === "?") {
      this.all = !this.all;
      this.repeat = false;
      this.generation++;
      cx.notify();
      return;
    }
    const binding = plain && BINDINGS.find((b) => b[0] === key);
    if (!binding) {
      const forwarded = this.repeat && plain && forwardKey(e, key);
      if (forwarded) return this.send(forwarded, cx);
      return this.dismiss();
    }
    const [, , command, args = {}, repeatable = false] = binding;
    if (!repeatable) this.dismiss();
    run(command, args);
    if (repeatable) {
      this.repeat = true;
      const generation = ++this.generation;
      cx.timer.after(REPEAT_MS, () => {
        if (this.generation === generation) this.dismiss();
      });
      cx.notify();
    }
  }

  render() {
    const t = theme();
    const rows = this.all
      ? BINDINGS
      : BINDINGS.filter(([key]) =>
        !["left", "right", "up", "down"].includes(key) && !/^\d$/.test(key)
      );
    return v_flex().id("tmux-prefix").track_focus(this.handle)
      .on_key_down((e, cx) => this.key(e, cx))
      .p(14).gap(8).bg(t.popover).text_color(t.foreground).text_size(12)
      .child(
        h_flex().justify_between().child(
          div().font_semibold().child(
            this.repeat
              ? `Repeat · ${REPEAT_MS} ms`
              : "Ctrl+Space · waiting for a key",
          ),
        )
          .child(
            div().text_color(t.muted).child("Esc cancel · ^Space send prefix"),
          ),
      )
      .child(
        v_flex().h(280).min_h(0).overflow_y_scrollbar().children(
          rows.map(([key, label, , , repeatable]) =>
            h_flex().h(20).flex_none().items_center().gap(12).child(
              div().w(40).text_color(t.accent).child(key),
            )
              .child(div().flex_1().child(label)).child(
                div().text_color(t.dim).child(repeatable ? "repeat" : ""),
              )
          ),
        ),
      )
      .child(
        div().text_color(t.muted).child(
          "0–9 tabs (0 = 10) · arrows focus · ? all bindings",
        ),
      );
  }
}
