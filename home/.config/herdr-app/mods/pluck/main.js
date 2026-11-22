// Copy a token from the focused terminal's screen by typing its hint.
// Port of the herdr-pluck TUI plugin (itself a tmux-thumbs replacement).

import { div, View } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { closeOverlay, focused, hc, notify, props, theme } from "herdr";
import { hints, tokens } from "./tokens.js";

// Popups are as tall as their content, so the list needs a height of its own.
const LIST_HEIGHT = 400;

export default class Pluck extends View {
  init(_props, cx) {
    this.handle = cx.focus_handle();
    this.handle.focus();
    this.typed = "";
    this.items = null;
    const terminal = focused()?.terminal;
    cx.spawn(async (cx) => {
      const r = terminal
        ? await hc(["read", terminal])
        : { code: 1, stdout: "", stderr: "no terminal has focus" };
      if (r.code !== 0) this.error = r.stderr.trim();
      const found = tokens(r.stdout);
      const keys = hints(found.length);
      this.items = found.map((text, i) => ({ text, hint: keys[i] }));
      cx.notify();
    });
  }

  dismiss() {
    closeOverlay(props().overlay);
  }

  key(e, cx) {
    cx.stop_propagation();
    if (e.key === "escape") return this.dismiss();
    if (e.key === "backspace") this.typed = this.typed.slice(0, -1);
    else if (/^[a-z]$/.test(e.key)) this.typed += e.key;
    const left = (this.items ?? []).filter((i) =>
      i.hint.startsWith(this.typed)
    );
    const hit = left.find((i) => i.hint === this.typed);
    if (hit) {
      cx.write_to_clipboard(hit.text);
      notify({ title: "Copied", body: hit.text, timeout: 1500 });
      return this.dismiss();
    }
    if (!left.length) this.typed = "";
    cx.notify();
  }

  render() {
    const t = theme();
    const rows = (this.items ?? []).filter((i) =>
      i.hint.startsWith(this.typed)
    );
    const status = this.error ??
      (this.items === null ? "Reading…" : "Nothing to pluck on this screen");
    return v_flex().id("pluck").track_focus(this.handle)
      .on_key_down((e, cx) => this.key(e, cx))
      .p(14).gap(8).bg(t.popover).text_color(t.foreground)
      .text_size(12)
      .child(
        h_flex().justify_between()
          .child(div().font_semibold().child("Pluck · type a hint to copy"))
          .child(div().text_color(t.muted).child("Esc cancel")),
      )
      .child(
        v_flex().h(LIST_HEIGHT).min_h(0).overflow_y_scrollbar()
          .when(
            !rows.length,
            (el) => el.child(div().text_color(t.muted).child(status)),
          )
          .children(
            rows.map(({ hint, text }) =>
              h_flex().h(20).flex_none().items_center().gap(12)
                .child(
                  div().w(28).flex_none().font_semibold().text_color(t.accent)
                    .child(hint),
                )
                .child(div().flex_1().min_w(0).truncate().child(text))
            ),
          ),
      );
  }
}
