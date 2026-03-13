"""GTK4 Layer Shell overlay for interactive text selection."""

from __future__ import annotations

import subprocess
from pathlib import Path

import cairo
import gi

gi.require_version("Gdk", "4.0")
gi.require_version("Gtk", "4.0")
gi.require_version("Gtk4LayerShell", "1.0")

from gi.repository import Gdk, GLib, Gtk, Gtk4LayerShell  # noqa: E402

from .ocr import LineBox  # noqa: E402

# Colors (RGBA)
HOVER_FILL = (0.2, 0.6, 1.0, 0.25)
HOVER_BORDER = (0.2, 0.6, 1.0, 0.7)
SELECTED_FILL = (0.2, 0.6, 1.0, 0.4)
SELECTED_BORDER = (0.2, 0.6, 1.0, 0.8)
FLASH_FILL = (1.0, 1.0, 1.0, 0.6)
FLASH_BORDER = (1.0, 1.0, 1.0, 0.9)
DRAG_FILL = (0.2, 0.6, 1.0, 0.12)
DRAG_BORDER = (0.2, 0.6, 1.0, 0.5)
TEXT_BG = (0.0, 0.0, 0.0, 0.03)

# Hint bar
HINT_BG = (0.0, 0.0, 0.0, 0.55)
HINT_TEXT_COLOR = (1.0, 1.0, 1.0)
HINT_FONT_SIZE = 13.0
HINT_PADDING = 8.0

LINE_PADDING = 2
MIN_DRAG_DISTANCE = 5.0
FLASH_DURATION_MS = 150


def _line_rect(line: LineBox) -> tuple[float, float, float, float]:
    """Return (x, y, w, h) for a line box including padding."""
    return (
        line.x - LINE_PADDING,
        line.y - LINE_PADDING,
        line.width + 2 * LINE_PADDING,
        line.height + 2 * LINE_PADDING,
    )


class LiveTextOverlay:
    """Full-screen overlay showing screenshot with selectable OCR text regions.

    Interaction model (familiar from text editors / macOS Live Text):
      - Click a line → select it (replaces previous selection)
      - Shift+click  → add/remove line from selection
      - Drag          → select all lines in rectangle
      - Ctrl+C / Enter → copy selection to clipboard
      - Ctrl+A        → select all
      - Click empty   → clear selection
      - Escape        → quit
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

        self.hovered_line: LineBox | None = None
        self.selected_lines: set[int] = set()

        # Drag state
        self.dragging = False
        self.drag_start_x = 0.0
        self.drag_start_y = 0.0
        self.drag_current_x = 0.0
        self.drag_current_y = 0.0
        self._drag_exceeded_threshold = False
        # Whether Shift was held when the drag/click started
        self._drag_shift = False

        # Copy-flash animation
        self._flashing = False

        self._image_surface: cairo.ImageSurface | None = None

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

        drawing_area = Gtk.DrawingArea()
        drawing_area.set_draw_func(self._draw)
        drawing_area.set_hexpand(True)
        drawing_area.set_vexpand(True)
        self._drawing_area = drawing_area

        motion = Gtk.EventControllerMotion()
        motion.connect("motion", self._on_motion)
        drawing_area.add_controller(motion)

        drag = Gtk.GestureDrag()
        drag.connect("drag-begin", self._on_drag_begin)
        drag.connect("drag-update", self._on_drag_update)
        drag.connect("drag-end", self._on_drag_end)
        drawing_area.add_controller(drag)

        keys = Gtk.EventControllerKey()
        keys.connect("key-pressed", self._on_key_pressed)
        window.add_controller(keys)

        drawing_area.set_cursor(Gdk.Cursor.new_from_name("crosshair"))
        window.set_child(drawing_area)
        window.present()

    # -- coordinate mapping ---------------------------------------------------

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

        img_w = self._image_surface.get_width()
        img_h = self._image_surface.get_height()
        scale, ox, oy = self._get_scale(width, height)

        # Screenshot + dim
        cr.save()
        cr.translate(ox, oy)
        cr.scale(scale, scale)
        cr.set_source_surface(self._image_surface, 0, 0)
        cr.paint()
        cr.set_source_rgba(0, 0, 0, 0.15)
        cr.rectangle(0, 0, img_w, img_h)
        cr.fill()

        # Subtle text region backgrounds
        for line in self.lines:
            cr.set_source_rgba(*TEXT_BG)
            cr.rectangle(*_line_rect(line))
            cr.fill()

        # Selected lines (flash white briefly after copy)
        fill = FLASH_FILL if self._flashing else SELECTED_FILL
        border = FLASH_BORDER if self._flashing else SELECTED_BORDER
        for idx in self.selected_lines:
            rect = _line_rect(self.lines[idx])
            cr.set_source_rgba(*fill)
            cr.rectangle(*rect)
            cr.fill()
            cr.set_source_rgba(*border)
            cr.set_line_width(2.0 / scale)
            cr.rectangle(*rect)
            cr.stroke()

        # Hovered line (only if not already selected — avoid double-highlight)
        if self.hovered_line is not None:
            hi = next(
                (i for i, ln in enumerate(self.lines) if ln is self.hovered_line),
                None,
            )
            if hi is not None and hi not in self.selected_lines:
                rect = _line_rect(self.hovered_line)
                cr.set_source_rgba(*HOVER_FILL)
                cr.rectangle(*rect)
                cr.fill()
                cr.set_source_rgba(*HOVER_BORDER)
                cr.set_line_width(2.0 / scale)
                cr.rectangle(*rect)
                cr.stroke()

        cr.restore()

        # Drag rectangle (widget coords)
        if self.dragging and self._drag_exceeded_threshold:
            rx = min(self.drag_start_x, self.drag_current_x)
            ry = min(self.drag_start_y, self.drag_current_y)
            rw = abs(self.drag_current_x - self.drag_start_x)
            rh = abs(self.drag_current_y - self.drag_start_y)
            cr.set_source_rgba(*DRAG_FILL)
            cr.rectangle(rx, ry, rw, rh)
            cr.fill()
            cr.set_source_rgba(*DRAG_BORDER)
            cr.set_line_width(1.5)
            cr.rectangle(rx, ry, rw, rh)
            cr.stroke()

        # Bottom hint bar
        self._draw_hint_bar(cr, width, height)

    def _draw_hint_bar(self, cr: cairo.Context, width: int, height: int) -> None:
        n = len(self.selected_lines)
        if n > 0:
            hint = (
                f"{n} line{'s' if n != 1 else ''} selected  ·  Ctrl+C copy  ·  Esc quit"
            )
        else:
            hint = "Click or drag to select text  ·  Ctrl+A select all  ·  Esc quit"

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
        old = self.hovered_line
        self.hovered_line = next(
            (ln for ln in self.lines if ln.contains_point(ix, iy)), None
        )
        if old != self.hovered_line:
            self._drawing_area.queue_draw()

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

        # Check if Shift is held at press time
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

        # Select lines intersecting the drag rectangle
        ix1, iy1 = self._widget_to_image(
            min(self.drag_start_x, self.drag_current_x),
            min(self.drag_start_y, self.drag_current_y),
        )
        ix2, iy2 = self._widget_to_image(
            max(self.drag_start_x, self.drag_current_x),
            max(self.drag_start_y, self.drag_current_y),
        )

        self.selected_lines.clear()
        for i, line in enumerate(self.lines):
            if line.intersects_rect(int(ix1), int(iy1), int(ix2 - ix1), int(iy2 - iy1)):
                self.selected_lines.add(i)

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

        # Click (not a real drag)
        rx = self.drag_start_x + offset_x
        ry = self.drag_start_y + offset_y
        ix, iy = self._widget_to_image(rx, ry)

        clicked = next(
            (i for i, ln in enumerate(self.lines) if ln.contains_point(ix, iy)),
            None,
        )

        if clicked is not None:
            if self._drag_shift:
                # Shift+click: toggle this line in the selection
                self.selected_lines.symmetric_difference_update({clicked})
            else:
                # Plain click: select only this line
                self.selected_lines = {clicked}
        else:
            # Clicked empty area: clear selection
            self.selected_lines.clear()

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
            self.selected_lines = set(range(len(self.lines)))
            self._drawing_area.queue_draw()
            return True

        return False

    # -- clipboard ------------------------------------------------------------

    def _end_flash(self) -> bool:
        self._flashing = False
        self._drawing_area.queue_draw()
        return False

    def _copy_selection(self) -> None:
        """Copy selected text to clipboard with visual flash feedback."""
        if not self.selected_lines:
            return

        selected = sorted(
            self.selected_lines,
            key=lambda i: (self.lines[i].y, self.lines[i].x),
        )
        text = "\n".join(self.lines[i].text for i in selected)

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
