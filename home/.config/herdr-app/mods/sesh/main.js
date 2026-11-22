// Jump to a project: open spaces first, then zoxide's directories, which
// become a new space. Port of the herdr-plugin-sesh picker.

import { div, View } from "gpui-kit";
import { h_flex, Input, InputState, ScrollHandle, v_flex } from "gpui-base";
import { closeOverlay, exec, home, props, run, spaces, theme } from "herdr";

const SHOWN = 60;
// Popups are as tall as their content, so the list needs a height of its own.
const LIST_HEIGHT = 360;

export default class Sesh extends View {
  init(_props, cx) {
    this.dirs = [];
    this.at = 0;
    this.scroll = ScrollHandle.new();
    this.input = InputState.new({ placeholder: "Space or directory…" });
    this.input.on("change", (_e, cx) => {
      this.at = 0;
      cx.notify();
    });
    this.input.on("submit", () => this.go(this.rows()[this.at]));
    this.input.focus();
    cx.spawn(async (cx) => {
      try {
        const r = await exec("zoxide", ["query", "--list"]);
        if (r.code === 0) this.dirs = r.stdout.split("\n").filter(Boolean);
      } catch (e) {
        this.error = `zoxide: ${e}`;
      }
      cx.notify();
    });
  }

  rows() {
    const terms = this.input.value().toLowerCase().split(/\s+/).filter(Boolean);
    const match = (s) => terms.every((w) => s.toLowerCase().includes(w));
    const open = spaces();
    // A directory that already is a local space is listed as that space.
    const taken = new Set(open.filter((s) => !s.machine).map((s) => s.folder));
    return [
      ...open.filter((s) => match(`${s.name} ${s.folder}`))
        .map((space) => ({ space })),
      ...this.dirs.filter((d) => !taken.has(d) && match(d))
        .map((dir) => ({ dir })),
    ].slice(0, SHOWN);
  }

  go(row) {
    if (!row) return;
    closeOverlay(props().overlay);
    if (row.space) return run("space.select", { id: row.space.id });
    run("space.new", { folder: row.dir, machine: "" });
    // Not awaited: the view is gone, the process still runs.
    exec("zoxide", ["add", row.dir]);
  }

  render() {
    const t = theme();
    const rows = this.rows();
    this.at = Math.max(0, Math.min(this.at, rows.length - 1));
    const tilde = (p) =>
      p.startsWith(home()) ? `~${p.slice(home().length)}` : p;
    return v_flex().id("sesh").p(14).gap(8).bg(t.popover)
      .text_color(t.foreground).text_size(12)
      .on_key_down((e, cx) => {
        const n = rows.length;
        const ctrl = e.modifiers.control;
        if (e.key === "escape") closeOverlay(props().overlay);
        else if (e.key === "down" || (ctrl && e.key === "n")) {
          this.at = (this.at + 1) % n;
        } else if (e.key === "up" || (ctrl && e.key === "p")) {
          this.at = (this.at + n - 1) % n;
        } else return;
        this.scroll.scroll_to_item(this.at);
        cx.stop_propagation();
        cx.notify();
      })
      .child(
        h_flex().h(32).flex_none().px(10).items_center().rounded(8).border_1()
          .border_color(t.accent).bg(t.background)
          .child(Input.new(this.input).flex_1().h_full()),
      )
      .when(
        Boolean(this.error),
        (el) => el.child(div().text_color(t.blocked).child(this.error)),
      )
      .child(
        v_flex().id("sesh-list").h(LIST_HEIGHT).min_h(0).overflow_y_scrollbar()
          .track_scroll(this.scroll)
          .children(
            rows.map((row, i) =>
              h_flex().id(`sesh-${i}`).h(24).flex_none().px(8).gap(10)
                .items_center().rounded(5).cursor_pointer()
                .when(i === this.at, (el) => el.bg(t.selected))
                .hover((s) => s.bg(t.hover))
                .on_click(() => this.go(row))
                .child(
                  div().w(12).flex_none()
                    .text_color(row.space ? t.accent : t.dim)
                    .child(row.space ? "●" : "+"),
                )
                .child(
                  div().flex_none().child(
                    row.space ? row.space.name : tilde(row.dir),
                  ),
                )
                .child(
                  div().flex_1().min_w(0).truncate().text_color(t.dim).child(
                    row.space
                      ? [row.space.machine, tilde(row.space.folder)]
                        .filter(Boolean).join(":")
                      : "",
                  ),
                )
            ),
          ),
      );
  }
}
