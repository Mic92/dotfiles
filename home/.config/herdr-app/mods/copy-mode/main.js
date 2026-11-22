import { div, View } from "gpui-kit";
import {
  closeOverlay,
  focused,
  hc,
  nextCommand,
  panes,
  props,
  run,
  terminals,
} from "herdr";
import { terminalText } from "./text.js";
import { transitions } from "./transitions.js";

const MAX_HISTORY_ROWS = 1000;
const shifted = { "4": "$", "6": "^", "[": "{", "]": "}" };
let session = 0;

function keyName({ key, key_char, modifiers }) {
  if (modifiers.alt || modifiers.platform) return null;
  if (modifiers.control) return modifiers.shift ? null : `ctrl-${key}`;
  if (key === "space" || key_char === " ") return "space";
  const ch = key_char || key;
  return modifiers.shift && ch.length === 1
    ? shifted[ch] ?? ch.toUpperCase()
    : ch;
}

function sameCell(a, b) {
  return a?.row === b.row && a.col === b.col;
}

const clamp = (n, min, max) => Math.max(min, Math.min(n, max));
const sameGeometry = (a, b) =>
  a.cols === b.cols && a.height === b.height && a.primary === b.primary;

export default class CopyMode extends View {
  init(_props, cx) {
    const overlay = props()?.overlay;
    if (!overlay) return; // `herdr-app check` loads without an overlay.
    this.overlay = overlay;
    this.mode = "move";
    const { terminal, pane, shown } = focused() ?? {};
    if (
      !terminal || !pane || shown ||
      !terminals().some((t) => t.address === terminal && t.state === "live")
    ) {
      closeOverlay(overlay);
      return;
    }

    this.terminal = terminal;
    this.pane = pane;
    this.queue = [];
    this.syncKey = `selection-sync-${++session}`;
    cx.spawn(async () => {
      while (!this.finished) {
        if (await nextCommand() === this.syncKey && !this.finished) {
          const acknowledge = this.acknowledge;
          this.acknowledge = null;
          acknowledge?.();
        }
      }
    });
    const scroll = this.scroll();
    this.entryOffset = scroll?.from_bottom;
    this.handle = cx.focus_handle();
    this.handle.focus();
    // The program cursor may not be visible in an already scrolled pane.
    const at = scroll?.from_bottom
      ? { row: scroll.offset + scroll.len - 1, col: 0 }
      : "cursor";
    this.entryGrid = this.grid();
    this.select(at);
    this.ready = this.initializeCursor();
  }

  async initializeCursor() {
    await this.flush();
    this.cursor = this.selection()?.start;
  }

  flush() {
    // On Desktop 0.1.16, run() posts to the window's FIFO. A command routed
    // back to this copy arrives after earlier edits publish their endpoints.
    return new Promise((resolve) => {
      this.acknowledge = resolve;
      run(`copy-mode.${this.syncKey}`);
    });
  }

  target() {
    if (this.finished) return null;
    const current = focused();
    return current?.pane === this.pane && current.terminal === this.terminal &&
        !current.shown
      ? current
      : null;
  }

  selection() {
    return this.target()?.selection;
  }

  scroll() {
    const pane = panes().find((p) => p.pane === this.pane);
    return pane?.terminal === this.terminal ? pane.scroll : null;
  }

  grid() {
    const terminal = terminals().find((t) => t.address === this.terminal);
    const scroll = this.scroll();
    const cols = terminal?.cols;
    const height = scroll?.len ?? terminal?.rows;
    if (!cols || !height) return null;
    return {
      cols,
      height,
      total: scroll?.total ?? height,
      offset: scroll?.offset ?? 0,
      primary: !!scroll,
    };
  }

  select(at) {
    run("terminal.select", { terminal: this.terminal, at });
  }

  adjust(direction) {
    run("terminal.adjust_selection", { terminal: this.terminal, direction });
  }

  scrollTo(offset) {
    const scroll = this.scroll();
    if (scroll && scroll.offset !== offset) {
      run("terminal.scroll", {
        terminal: this.terminal,
        lines: offset - scroll.offset,
      });
      return true;
    }
    return false;
  }

  cleanup() {
    if (!this.terminal || this.finished) return;
    this.finished = true;
    this.acknowledge?.();
    run("terminal.clear_selection", { terminal: this.terminal });
    // Restore from a known origin, even if Esc closed the popup with a native
    // edit still in flight. The desktop runs both requests after prior edits.
    if (this.entryOffset != null && this.scroll()) {
      run("terminal.scroll", { terminal: this.terminal, to: "bottom" });
      if (this.entryOffset > 0) {
        run("terminal.scroll", {
          terminal: this.terminal,
          lines: -this.entryOffset,
        });
      }
    }
  }

  async close() {
    this.cleanup();
  }

  key(event, cx) {
    cx.stop_propagation();
    if (this.finished) return;
    const key = keyName(event);
    if (key == null) return;
    this.queue.push(key);
    if (this.draining) return;
    this.draining = true;
    cx.spawn(async () => {
      try {
        await this.ready;
        while (!this.finished && this.queue.length) {
          await this.apply(this.queue.shift());
        }
      } finally {
        this.draining = false;
      }
    });
  }

  async readText(grid) {
    let adjusted = false;
    const sameGrid = () => {
      const now = this.grid();
      return this.target() && now && sameGeometry(grid, now) &&
        now.total === grid.total;
    };
    try {
      // A repaint can change every word without changing the grid geometry.
      // A history read on the alternate screen can send scroll input to the PTY.
      const lines = grid.primary
        ? Math.min(MAX_HISTORY_ROWS, grid.total - grid.height)
        : 0;
      const { code, stdout } = await hc([
        "read",
        this.terminal,
        "--json",
        "--lines",
        String(lines),
      ]);
      if (code !== 0 || !sameGrid()) return null;
      const { how, text } = JSON.parse(stdout);
      if (typeof text !== "string") return null;
      if (how === "screen") {
        // The screen read preserves leading blank rows, but drops trailing
        // whitespace-only rows even when the native grid has text cells there.
        return {
          text: terminalText(text, grid.cols, grid.total, {
            first: grid.total - grid.height,
          }),
        };
      } else if (grid.primary && how === "history") {
        // History may return fewer rows than requested. Measure its bottom
        // against the live grid rather than keeping an endpoint from entry.
        this.adjust("end");
        adjusted = true;
        await this.flush();
        const last = this.selection()?.end.row;
        if (last != null && sameGrid()) {
          return {
            text: terminalText(text, grid.cols, grid.total, {
              last: text ? last : -1,
            }),
            adjusted,
          };
        }
      }
    } catch {
      // A failed text read leaves the copy cursor where it was.
    }
    return { adjusted };
  }

  moveUp(distance, height) {
    for (; distance >= height; distance -= height) this.adjust("page_up");
    for (; distance > 0; distance--) this.adjust("up");
  }

  async renderCursor(grid, offset) {
    const cursor = this.cursor;
    if (this.mode === "move") {
      this.select(cursor);
      this.scrollTo(offset);
      await this.flush();
      return;
    }
    const anchor = this.anchor;
    const line = this.mode === "line";
    let [start, end] = anchor.row >= cursor.row
      ? [anchor, cursor]
      : [cursor, anchor];
    if (line) {
      start = { row: start.row, col: grid.cols - 1 };
      end = { row: end.row, col: 0 };
    }

    // Native `down` skips blank rows; build from the lower cell on screen.
    // A backwards native range still represents the same logical visual anchor.
    const selection = this.selection();
    const reuse = sameCell(selection?.start, start) &&
      selection.end.row >= end.row;
    const from = reuse ? selection.end : start;
    if (!reuse) this.select(start);
    this.moveUp(from.row - end.row, grid.height);
    const edge = end.col === 0 || end.col === grid.cols - 1;
    if (edge && from.col !== end.col) {
      this.adjust(end.col ? "line_end" : "line_start");
    }
    await this.flush();
    if (!this.selection()) return;
    if (
      !edge && from.col !== end.col &&
      !await this.seekColumn(end, start, grid) && this.target()
    ) {
      // An unoccupied cell can only be selected directly, not reached by
      // native left/right. For a same-row range, try making it the fixed end.
      if (start.row === end.row) {
        this.select(end);
        await this.flush();
        if (!await this.seekColumn(start, end, grid) && this.target()) {
          this.select(start);
          await this.flush();
        }
      } else {
        const row = this.text?.row(end.row);
        // A trailing padded cell includes the same text as the right edge.
        // Use the last column for ranges through padding, not the next row.
        this.select(start);
        this.moveUp(start.row - end.row, grid.height);
        if (row && end.col > (row.at(-1)?.col ?? -1)) this.adjust("line_end");
        await this.flush();
      }
    }
    if (this.target() && this.scrollTo(offset)) await this.flush();
  }

  async seekColumn(target, fixed, grid) {
    if (target.col === 0 || target.col === grid.cols - 1) {
      if (this.selection()?.end.row !== target.row) return false;
      this.adjust(target.col ? "line_end" : "line_start");
      await this.flush();
      return true;
    }
    let previousDirection = 0;
    let reversed = false;
    for (let tries = 0; tries < grid.cols && this.target(); tries++) {
      const at = this.selection()?.end;
      if (sameCell(at, target)) return true;
      if (!at || at.row !== target.row) return false;
      const dir = target.col > at.col ? 1 : -1;
      if (previousDirection && dir !== previousDirection) {
        if (reversed) return false;
        reversed = true;
      }
      previousDirection = dir;
      const low = Math.min(at.col, target.col);
      const high = Math.max(at.col, target.col);
      // Count only nonblank cells for the first batch. Ghostty may skip
      // implicit blank cells but not occupied characters.
      const row = this.text?.row(target.row);
      const count = row?.filter(({ col, kind }) =>
        kind && (dir > 0 ? col > low && col <= high : col >= low && col < high)
      ).length ?? 0;
      for (let i = 0; i < Math.max(1, count); i++) {
        this.adjust(dir > 0 ? "right" : "left");
      }
      await this.flush();
      const moved = this.selection();
      if (
        !moved || !sameCell(moved.start, fixed) || sameCell(moved.end, at) ||
        moved.end.row !== target.row
      ) {
        return false;
      }
      if ((moved.end.col - target.col) * dir > 0 && count <= 1) {
        return false;
      }
    }
    return false;
  }

  async apply(key) {
    const grid = this.grid();
    if (!grid || !this.cursor || !this.selection()) {
      this.finish("cancel");
      return;
    }
    if (this.entryGrid && !sameGeometry(this.entryGrid, grid)) {
      this.finish("cancel");
      return;
    }
    const table = transitions[this.mode];
    const objectKey = this.prefix && this.prefix + key;
    this.prefix = null;
    if (
      objectKey && Object.hasOwn(table, objectKey) &&
      table[objectKey][1] === "object"
    ) key = objectKey;
    if (!Object.hasOwn(table, key)) return;
    const [mode, action, direction] = table[key];
    if (action === "prefix") {
      this.prefix = direction;
      return;
    }
    if (action !== "object") this.mode = mode;
    const at = this.cursor;
    let offset = grid.offset;
    switch (action) {
      case "anchor":
      case "anchor_line":
        this.anchor = at;
        break;
      case "text":
      case "object": {
        const snapshot = await this.readText(grid);
        if (!this.target()) return;
        const now = this.grid();
        if (!now || !sameGeometry(grid, now)) {
          this.finish("cancel");
          return;
        }
        this.text = snapshot?.text;
        if (action === "object") {
          const range = this.text?.object(at, direction);
          if (range && range.end.row - range.start.row < MAX_HISTORY_ROWS) {
            this.mode = mode;
            this.anchor = range.start;
            this.cursor = range.end;
          } else if (!snapshot?.adjusted) return;
        } else {
          const next = this.text?.motion(at, direction);
          if (next) this.cursor = next;
          else if (!snapshot?.adjusted) return;
        }
        // A history probe moved the native endpoint even if there was no match.
        break;
      }
      case "move": {
        let { row, col } = at;
        switch (direction) {
          case "left":
            col--;
            break;
          case "right":
            col++;
            break;
          case "up":
            row--;
            break;
          case "down":
            row++;
            break;
          case "line_start":
            col = 0;
            break;
          case "home":
            row = 0;
            offset = 0;
            break;
          case "end":
            row = grid.total - 1;
            offset = grid.total - grid.height;
            break;
          default: {
            const half = direction.startsWith("half");
            const size = Math.max(
              1,
              half ? Math.floor(grid.height / 2) : grid.height - 2,
            );
            const step = direction.endsWith("up") ? -size : size;
            row += step;
            offset += step;
          }
        }
        this.cursor = {
          row: clamp(row, 0, grid.total - 1),
          col: clamp(col, 0, grid.cols - 1),
        };
        if (sameCell(at, this.cursor) && offset === grid.offset) return;
        break;
      }
      default:
        this.finish(action);
        return;
    }
    if (this.anchor && this.mode !== "move") {
      const min = Math.max(0, this.anchor.row - MAX_HISTORY_ROWS + 1);
      const max = Math.min(
        grid.total - 1,
        this.anchor.row + MAX_HISTORY_ROWS - 1,
      );
      this.cursor = { ...this.cursor, row: clamp(this.cursor.row, min, max) };
    }
    offset = clamp(offset, this.cursor.row - grid.height + 1, this.cursor.row);
    await this.renderCursor(grid, clamp(offset, 0, grid.total - grid.height));
  }

  finish(action) {
    if (action === "yank") run("terminal.copy");
    this.cleanup();
    closeOverlay(this.overlay);
    if (action === "search") run("terminal.search");
  }

  render() {
    const root = div().id("copy-mode").w(0).h(0);
    return this.handle
      ? root.track_focus(this.handle).on_key_down((event, cx) =>
        this.key(event, cx)
      )
      : root;
  }
}
