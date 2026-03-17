"""GTK4 Layer Shell overlay for interactive text selection.

Renders the screenshot as-is.  Text is selectable at the word level:
the cursor changes to an I-beam over detected words, and dragging
paints a translucent blue highlight behind each selected word — like
macOS Live Text or a PDF viewer.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

import cairo
import gi

gi.require_version("Gdk", "4.0")
gi.require_version("Gtk", "4.0")
gi.require_version("Gtk4LayerShell", "1.0")

from gi.repository import Gdk, GLib, Gtk, Gtk4LayerShell  # noqa: E402

from .ocr import LineBox, WordBox  # noqa: E402

# Selection highlight — macOS-style blue text selection
SELECTION_FILL = (0.26, 0.52, 0.96, 0.35)
FLASH_FILL = (0.26, 0.52, 0.96, 0.6)

# Hint bar
HINT_BG = (0.0, 0.0, 0.0, 0.55)
HINT_TEXT_COLOR = (1.0, 1.0, 1.0)
HINT_FONT_SIZE = 13.0
HINT_PADDING = 8.0

MIN_DRAG_DISTANCE = 5.0
FLASH_DURATION_MS = 150
WORD_PADDING = 1


def _word_rect(w: WordBox) -> tuple[float, float, float, float]:
    """Return (x, y, width, height) for a word with padding."""
    return (
        w.x - WORD_PADDING,
        w.y - WORD_PADDING,
        w.width + 2 * WORD_PADDING,
        w.height + 2 * WORD_PADDING,
    )


# A selected word is identified by (line_index, word_index).
WordId = tuple[int, int]


class LiveTextOverlay:
    """Full-screen overlay — screenshot with word-level text selection.

    Interaction model (like macOS Live Text / PDF viewers):
      - Cursor becomes I-beam over text, default arrow elsewhere
      - Click a word to select it
      - Drag to select words in the swept area
      - Shift+click to extend/shrink selection
      - Ctrl+C / Enter to copy
      - Ctrl+A to select all
      - Escape to quit
    """

    def __init__(
        self,
        screenshot_path: Path,
        lines: list[LineBox],
        wl_copy_cmd: str = "wl-copy",
    ) -> None:
        self.screenshot_path = screenshot_path
        self.lines = lines
        self.wl_copy_cmd = wl_copy_cmd

        # Selected words as (line_idx, word_idx) pairs
        self.selected_words: set[WordId] = set()
        self._over_text = False

        # Drag state
        self.dragging = False
        self.drag_start_x = 0.0
        self.drag_start_y = 0.0
        self.drag_current_x = 0.0
        self.drag_current_y = 0.0
        self._drag_exceeded_threshold = False
        self._drag_shift = False

        # Copy-flash animation
        self._flashing = False

        self._image_surface: cairo.ImageSurface | None = None
        self._cursor_text = Gdk.Cursor.new_from_name("text")
        self._cursor_default = Gdk.Cursor.new_from_name("default")

        self.app = Gtk.Application(application_id="org.mic92.live-text")
        self.app.connect("activate", self._on_activate)

    def run(self) -> None:
        self.app.run([])

    # -- setup ----------------------------------------------------------------

    def _on_activate(self, app: Gtk.Application) -> None:
        window = Gtk.ApplicationWindow(application=app)

        Gtk4LayerShell.init_for_window(window)
        Gtk4LayerShell.set_layer(window, Gtk4LayerShell.Layer.OVERLAY)
        Gtk4LayerShell.set_keyboard_mode(window, Gtk4LayerShell.KeyboardMode.EXCLUSIVE)
        for edge in (
            Gtk4LayerShell.Edge.TOP,
            Gtk4LayerShell.Edge.BOTTOM,
            Gtk4LayerShell.Edge.LEFT,
            Gtk4LayerShell.Edge.RIGHT,
        ):
            Gtk4LayerShell.set_anchor(window, edge, True)
        Gtk4LayerShell.set_exclusive_zone(window, -1)

        self._image_surface = cairo.ImageSurface.create_from_png(
            str(self.screenshot_path)
        )

        da = Gtk.DrawingArea()
        da.set_draw_func(self._draw)
        da.set_hexpand(True)
        da.set_vexpand(True)
        self._drawing_area = da

        motion = Gtk.EventControllerMotion()
        motion.connect("motion", self._on_motion)
        da.add_controller(motion)

        drag = Gtk.GestureDrag()
        drag.connect("drag-begin", self._on_drag_begin)
        drag.connect("drag-update", self._on_drag_update)
        drag.connect("drag-end", self._on_drag_end)
        da.add_controller(drag)

        keys = Gtk.EventControllerKey()
        keys.connect("key-pressed", self._on_key_pressed)
        window.add_controller(keys)

        da.set_cursor(self._cursor_default)
        window.set_child(da)
        window.present()

    # -- coordinate helpers ---------------------------------------------------

    def _get_scale(
        self,
        widget_w: int | None = None,
        widget_h: int | None = None,
    ) -> tuple[float, float, float]:
        if widget_w is None or widget_h is None:
            alloc = self._drawing_area.get_allocation()
            widget_w = alloc.width
            widget_h = alloc.height

        if self._image_surface is None:
            return 1.0, 0.0, 0.0

        img_w = self._image_surface.get_width()
        img_h = self._image_surface.get_height()
        if img_w == 0 or img_h == 0:
            return 1.0, 0.0, 0.0

        scale = min(widget_w / img_w, widget_h / img_h)
        return scale, (widget_w - img_w * scale) / 2, (widget_h - img_h * scale) / 2

    def _widget_to_image(self, wx: float, wy: float) -> tuple[float, float]:
        scale, ox, oy = self._get_scale()
        return (wx - ox) / scale, (wy - oy) / scale

    def _hit_word(self, ix: float, iy: float) -> WordId | None:
        """Return (line_idx, word_idx) of the word at image coords, or None."""
        for li, line in enumerate(self.lines):
            for wi, word in enumerate(line.words):
                if word.contains_point(ix, iy):
                    return (li, wi)
        return None

    # -- drawing --------------------------------------------------------------

    def _draw(
        self,
        _area: Gtk.DrawingArea,
        cr: cairo.Context,
        width: int,
        height: int,
    ) -> None:
        if self._image_surface is None:
            return

        scale, ox, oy = self._get_scale(width, height)

        cr.save()
        cr.translate(ox, oy)
        cr.scale(scale, scale)
        cr.set_source_surface(self._image_surface, 0, 0)
        cr.paint()

        # Draw selection highlight per word (blue fill, no borders)
        if self.selected_words:
            fill = FLASH_FILL if self._flashing else SELECTION_FILL
            cr.set_source_rgba(*fill)
            for li, wi in self.selected_words:
                cr.rectangle(*_word_rect(self.lines[li].words[wi]))
            cr.fill()

        cr.restore()

        self._draw_hint_bar(cr, width, height)

    def _draw_hint_bar(self, cr: cairo.Context, width: int, height: int) -> None:
        n = len(self.selected_words)
        if n > 0:
            hint = (
                f"{n} word{'s' if n != 1 else ''} selected  ·  Ctrl+C copy  ·  Esc quit"
            )
        else:
            hint = "Select text  ·  Ctrl+A select all  ·  Esc quit"

        cr.set_font_size(HINT_FONT_SIZE)
        extents = cr.text_extents(hint)
        bar_h = extents.height + 2 * HINT_PADDING
        bar_y = height - bar_h

        cr.set_source_rgba(*HINT_BG)
        cr.rectangle(0, bar_y, width, bar_h)
        cr.fill()

        cr.set_source_rgb(*HINT_TEXT_COLOR)
        text_x = (width - extents.width) / 2 - extents.x_bearing
        text_y = bar_y + HINT_PADDING + extents.height
        cr.move_to(text_x, text_y)
        cr.show_text(hint)

    # -- mouse ----------------------------------------------------------------

    def _on_motion(
        self,
        _controller: Gtk.EventControllerMotion,
        x: float,
        y: float,
    ) -> None:
        ix, iy = self._widget_to_image(x, y)
        over = self._hit_word(ix, iy) is not None

        if over != self._over_text:
            self._over_text = over
            self._drawing_area.set_cursor(
                self._cursor_text if over else self._cursor_default
            )

    def _on_drag_begin(
        self,
        gesture: Gtk.GestureDrag,
        x: float,
        y: float,
    ) -> None:
        self.dragging = True
        self._drag_exceeded_threshold = False
        self.drag_start_x = x
        self.drag_start_y = y
        self.drag_current_x = x
        self.drag_current_y = y

        device = gesture.get_device()
        seat = device.get_seat() if device else None
        if seat is not None:
            mask = seat.get_keyboard().get_modifier_state()
            self._drag_shift = bool(mask & Gdk.ModifierType.SHIFT_MASK)
        else:
            self._drag_shift = False

    def _on_drag_update(
        self,
        _gesture: Gtk.GestureDrag,
        offset_x: float,
        offset_y: float,
    ) -> None:
        self.drag_current_x = self.drag_start_x + offset_x
        self.drag_current_y = self.drag_start_y + offset_y

        if (offset_x**2 + offset_y**2) ** 0.5 >= MIN_DRAG_DISTANCE:
            self._drag_exceeded_threshold = True

        if not self._drag_exceeded_threshold:
            return

        # Select all words intersecting the swept rectangle
        ix1, iy1 = self._widget_to_image(
            min(self.drag_start_x, self.drag_current_x),
            min(self.drag_start_y, self.drag_current_y),
        )
        ix2, iy2 = self._widget_to_image(
            max(self.drag_start_x, self.drag_current_x),
            max(self.drag_start_y, self.drag_current_y),
        )
        rw = int(ix2 - ix1)
        rh = int(iy2 - iy1)
        rx = int(ix1)
        ry = int(iy1)

        self.selected_words.clear()
        for li, line in enumerate(self.lines):
            for wi, word in enumerate(line.words):
                if word.intersects_rect(rx, ry, rw, rh):
                    self.selected_words.add((li, wi))

        self._drawing_area.queue_draw()

    def _on_drag_end(
        self,
        _gesture: Gtk.GestureDrag,
        offset_x: float,
        offset_y: float,
    ) -> None:
        was_drag = self._drag_exceeded_threshold
        self.dragging = False
        self._drag_exceeded_threshold = False

        if was_drag:
            self._drawing_area.queue_draw()
            return

        # Click
        rx = self.drag_start_x + offset_x
        ry = self.drag_start_y + offset_y
        ix, iy = self._widget_to_image(rx, ry)
        clicked = self._hit_word(ix, iy)

        if clicked is not None:
            if self._drag_shift:
                self.selected_words.symmetric_difference_update({clicked})
            else:
                self.selected_words = {clicked}
        else:
            self.selected_words.clear()

        self._drawing_area.queue_draw()

    # -- keyboard -------------------------------------------------------------

    def _on_key_pressed(
        self,
        _controller: Gtk.EventControllerKey,
        keyval: int,
        _keycode: int,
        state: Gdk.ModifierType,
    ) -> bool:
        ctrl = bool(state & Gdk.ModifierType.CONTROL_MASK)

        if keyval == Gdk.KEY_Escape:
            self.app.quit()
            return True

        if ctrl and keyval == Gdk.KEY_c:
            self._copy_selection()
            return True

        if keyval in (Gdk.KEY_Return, Gdk.KEY_KP_Enter):
            self._copy_selection()
            return True

        if ctrl and keyval == Gdk.KEY_a:
            self.selected_words = {
                (li, wi)
                for li, line in enumerate(self.lines)
                for wi in range(len(line.words))
            }
            self._drawing_area.queue_draw()
            return True

        return False

    # -- clipboard ------------------------------------------------------------

    def _end_flash(self) -> bool:
        self._flashing = False
        self._drawing_area.queue_draw()
        return False

    def _copy_selection(self) -> None:
        """Copy selected words to clipboard, grouped by line."""
        if not self.selected_words:
            return

        # Group selected words by line, preserving word order
        line_texts: dict[int, list[str]] = {}
        for li, wi in sorted(self.selected_words):
            line_texts.setdefault(li, []).append(self.lines[li].words[wi].text)

        text = "\n".join(" ".join(words) for words in line_texts.values())

        try:
            subprocess.run(
                [self.wl_copy_cmd],
                input=text.encode(),
                check=True,
            )
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            print(f"live-text: failed to copy to clipboard: {e}")
            return

        self._flashing = True
        self._drawing_area.queue_draw()
        GLib.timeout_add(FLASH_DURATION_MS, self._end_flash)
