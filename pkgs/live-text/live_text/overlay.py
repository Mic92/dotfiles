"""GTK4 Layer Shell overlay: text selection + annotation.

Single unified overlay that combines macOS Live Text-style word selection
with screenshot annotation (arrows, rectangles, text).  The toolbar at
the bottom lets you switch between Select mode and drawing tools.
"""

from __future__ import annotations

import enum
import math
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path

import cairo
import gi

gi.require_version("Gdk", "4.0")
gi.require_version("Gtk", "4.0")
gi.require_version("Gtk4LayerShell", "1.0")

from gi.repository import Gdk, GLib, Gtk, Gtk4LayerShell  # noqa: E402

from .ocr import LineBox, WordBox  # noqa: E402

# -- constants ----------------------------------------------------------------

# Text selection
SELECTION_FILL = (0.26, 0.52, 0.96, 0.35)
FLASH_FILL = (0.26, 0.52, 0.96, 0.6)
WORD_PADDING = 1
MIN_DRAG_DISTANCE = 5.0
FLASH_DURATION_MS = 150
SPINNER_FRAMES = "◐◓◑◒"
SPINNER_INTERVAL_MS = 120

# Annotation drawing
COLORS = [
    (1.0, 0.2, 0.2),  # red
    (0.2, 0.6, 1.0),  # blue
    (0.2, 0.8, 0.2),  # green
    (1.0, 0.8, 0.0),  # yellow
    (1.0, 1.0, 1.0),  # white
    (0.0, 0.0, 0.0),  # black
]
STROKE_WIDTH = 3.0
ARROW_HEAD_LEN = 18.0
ARROW_HEAD_ANGLE = math.pi / 7
ANN_FONT_SIZE = 20.0

# Toolbar
TOOLBAR_H = 40.0
TOOLBAR_BG = (0.15, 0.15, 0.15, 0.85)
TOOLBAR_TEXT_COLOR = (1.0, 1.0, 1.0)
TOOLBAR_ACTIVE_BG = (0.3, 0.5, 0.9, 0.9)
TOOL_BTN_W = 70.0
COLOR_BTN_SIZE = 22.0
HINT_FONT_SIZE = 12.0


# -- data types ---------------------------------------------------------------


class Tool(enum.Enum):
    SELECT = "Select"
    ARROW = "Arrow"
    RECT = "Rect"
    TEXT = "Text"


@dataclass
class Annotation:
    tool: Tool
    color: tuple[float, float, float]
    x1: float = 0.0
    y1: float = 0.0
    x2: float = 0.0
    y2: float = 0.0
    text: str = ""


WordId = tuple[int, int]


def _word_rect(w: WordBox) -> tuple[float, float, float, float]:
    return (
        w.x - WORD_PADDING,
        w.y - WORD_PADDING,
        w.width + 2 * WORD_PADDING,
        w.height + 2 * WORD_PADDING,
    )


# -- overlay ------------------------------------------------------------------


class LiveTextOverlay:
    """Unified overlay: OCR text selection + annotation drawing."""

    def __init__(
        self,
        screenshot_path: Path,
        lines: list[LineBox],
        wl_copy_cmd: str = "wl-copy",
    ) -> None:
        self.screenshot_path = screenshot_path
        self.lines = lines
        self.wl_copy_cmd = wl_copy_cmd

        # Current tool
        self.tool = Tool.SELECT

        # Text selection state
        self.selected_words: set[WordId] = set()
        self._over_text = False
        self._flashing = False

        # Annotation state
        self.color_idx = 0
        self.annotations: list[Annotation] = []
        self.current_ann: Annotation | None = None
        self._text_input = ""
        self._text_pos: tuple[float, float] = (0.0, 0.0)
        self._text_editing = False

        # Spinner for OCR progress
        self._spinner_idx = 0
        self._spinner_timer: int | None = None

        # Drag state (used by both select and annotation)
        self.dragging = False
        self.drag_start_x = 0.0
        self.drag_start_y = 0.0
        self.drag_current_x = 0.0
        self.drag_current_y = 0.0
        self._drag_exceeded_threshold = False
        self._drag_shift = False

        self._image_surface: cairo.ImageSurface | None = None
        self._cursor_ibeam = Gdk.Cursor.new_from_name("text")
        self._cursor_cross = Gdk.Cursor.new_from_name("crosshair")
        self._cursor_default = Gdk.Cursor.new_from_name("default")

        self.app = Gtk.Application(application_id="org.mic92.live-text")
        self.app.connect("activate", self._on_activate)

    @property
    def color(self) -> tuple[float, float, float]:
        return COLORS[self.color_idx]

    def set_lines(self, lines: list[LineBox]) -> None:
        """Update OCR lines from a background thread. Thread-safe."""
        GLib.idle_add(self._apply_lines, lines)

    def _apply_lines(self, lines: list[LineBox]) -> bool:
        """GLib.idle callback: apply OCR results on the main thread."""
        self.lines = lines
        if self._spinner_timer is not None:
            GLib.source_remove(self._spinner_timer)
            self._spinner_timer = None
        self._drawing_area.queue_draw()
        return False  # don't repeat

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

        # Start spinner if OCR hasn't finished yet
        if not self.lines:
            self._spinner_timer = GLib.timeout_add(
                SPINNER_INTERVAL_MS, self._tick_spinner
            )

    def _tick_spinner(self) -> bool:
        """Advance spinner frame and redraw toolbar."""
        self._spinner_idx = (self._spinner_idx + 1) % len(SPINNER_FRAMES)
        self._drawing_area.queue_draw()
        return True  # keep ticking

    # -- coordinates ----------------------------------------------------------

    def _get_scale(
        self, widget_w: int | None = None, widget_h: int | None = None
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

        usable_h = widget_h - int(TOOLBAR_H)
        scale = min(widget_w / img_w, usable_h / img_h)
        return scale, (widget_w - img_w * scale) / 2, (usable_h - img_h * scale) / 2

    def _widget_to_image(self, wx: float, wy: float) -> tuple[float, float]:
        scale, ox, oy = self._get_scale()
        return (wx - ox) / scale, (wy - oy) / scale

    def _hit_word(self, ix: float, iy: float) -> WordId | None:
        for li, line in enumerate(self.lines):
            for wi, word in enumerate(line.words):
                if word.contains_point(ix, iy):
                    return (li, wi)
        return None

    # -- drawing --------------------------------------------------------------

    def _draw(
        self, _area: Gtk.DrawingArea, cr: cairo.Context, width: int, height: int
    ) -> None:
        if self._image_surface is None:
            return

        scale, ox, oy = self._get_scale(width, height)

        # Dark background behind image
        cr.set_source_rgb(0.1, 0.1, 0.1)
        cr.paint()

        cr.save()
        cr.translate(ox, oy)
        cr.scale(scale, scale)
        cr.set_source_surface(self._image_surface, 0, 0)
        cr.paint()

        # Text selection highlight
        if self.selected_words:
            fill = FLASH_FILL if self._flashing else SELECTION_FILL
            cr.set_source_rgba(*fill)
            for li, wi in self.selected_words:
                cr.rectangle(*_word_rect(self.lines[li].words[wi]))
            cr.fill()

        # Committed annotations
        for ann in self.annotations:
            self._draw_annotation(cr, ann, scale)

        # In-progress annotation
        if self.current_ann is not None:
            self._draw_annotation(cr, self.current_ann, scale)

        # Text being typed (show cursor even when empty)
        if self._text_editing:
            cr.set_source_rgb(*self.color)
            cr.set_font_size(ANN_FONT_SIZE)
            cr.move_to(self._text_pos[0], self._text_pos[1])
            cr.show_text(self._text_input + "│")

        cr.restore()

        self._draw_toolbar(cr, width, height)

    def _draw_annotation(
        self, cr: cairo.Context, ann: Annotation, scale: float
    ) -> None:
        cr.set_source_rgb(*ann.color)
        cr.set_line_width(STROKE_WIDTH / scale)

        if ann.tool == Tool.ARROW:
            cr.move_to(ann.x1, ann.y1)
            cr.line_to(ann.x2, ann.y2)
            cr.stroke()
            angle = math.atan2(ann.y2 - ann.y1, ann.x2 - ann.x1)
            head = ARROW_HEAD_LEN / scale
            for sign in (-1, 1):
                cr.move_to(ann.x2, ann.y2)
                cr.line_to(
                    ann.x2 - head * math.cos(angle + sign * ARROW_HEAD_ANGLE),
                    ann.y2 - head * math.sin(angle + sign * ARROW_HEAD_ANGLE),
                )
                cr.stroke()
        elif ann.tool == Tool.RECT:
            cr.rectangle(
                min(ann.x1, ann.x2),
                min(ann.y1, ann.y2),
                abs(ann.x2 - ann.x1),
                abs(ann.y2 - ann.y1),
            )
            cr.stroke()
        elif ann.tool == Tool.TEXT:
            cr.set_font_size(ANN_FONT_SIZE)
            cr.move_to(ann.x1, ann.y1)
            cr.show_text(ann.text)

    def _draw_toolbar(self, cr: cairo.Context, width: int, height: int) -> None:
        bar_y = height - TOOLBAR_H

        cr.set_source_rgba(*TOOLBAR_BG)
        cr.rectangle(0, bar_y, width, TOOLBAR_H)
        cr.fill()

        # Tool buttons
        x = 10.0
        for tool in Tool:
            if tool == self.tool:
                cr.set_source_rgba(*TOOLBAR_ACTIVE_BG)
                cr.rectangle(x - 2, bar_y + 4, TOOL_BTN_W, TOOLBAR_H - 8)
                cr.fill()
            cr.set_source_rgb(*TOOLBAR_TEXT_COLOR)
            cr.set_font_size(14.0)
            ext = cr.text_extents(tool.value)
            cr.move_to(
                x + (TOOL_BTN_W - ext.width) / 2,
                bar_y + TOOLBAR_H / 2 + ext.height / 2,
            )
            cr.show_text(tool.value)
            x += TOOL_BTN_W + 6

        # Color swatches (only visible when not in Select mode)
        if self.tool != Tool.SELECT:
            x += 20
            for i, col in enumerate(COLORS):
                cy = bar_y + TOOLBAR_H / 2
                cx = x + i * (COLOR_BTN_SIZE + 4)
                cr.set_source_rgb(*col)
                cr.rectangle(
                    cx, cy - COLOR_BTN_SIZE / 2, COLOR_BTN_SIZE, COLOR_BTN_SIZE
                )
                cr.fill()
                if i == self.color_idx:
                    cr.set_source_rgb(1, 1, 1)
                    cr.set_line_width(2)
                    cr.rectangle(
                        cx - 2,
                        cy - COLOR_BTN_SIZE / 2 - 2,
                        COLOR_BTN_SIZE + 4,
                        COLOR_BTN_SIZE + 4,
                    )
                    cr.stroke()

        # Hint text
        if self.tool == Tool.SELECT:
            if not self.lines and self._spinner_timer is not None:
                frame = SPINNER_FRAMES[self._spinner_idx]
                hint = f"{frame} Detecting text…  · Ctrl+S save · Esc quit"
            elif not self.lines:
                hint = "No text detected  · Ctrl+S save · Esc quit"
            else:
                hint = "Ctrl+C copy text · Ctrl+S save · Ctrl+A select all · Esc quit"
        else:
            hint = "Ctrl+C copy image · Ctrl+S save · Ctrl+Z undo · Esc quit"
        cr.set_source_rgba(1, 1, 1, 0.6)
        cr.set_font_size(HINT_FONT_SIZE)
        ext = cr.text_extents(hint)
        cr.move_to(width - ext.width - 16, bar_y + TOOLBAR_H / 2 + ext.height / 2)
        cr.show_text(hint)

    # -- mouse ----------------------------------------------------------------

    def _on_motion(
        self, _controller: Gtk.EventControllerMotion, x: float, y: float
    ) -> None:
        if self.tool != Tool.SELECT:
            return
        ix, iy = self._widget_to_image(x, y)
        over = self._hit_word(ix, iy) is not None
        if over != self._over_text:
            self._over_text = over
            self._drawing_area.set_cursor(
                self._cursor_ibeam if over else self._cursor_default
            )

    def _on_drag_begin(self, gesture: Gtk.GestureDrag, x: float, y: float) -> None:
        # Toolbar click?
        alloc = self._drawing_area.get_allocation()
        if y >= alloc.height - TOOLBAR_H:
            self._handle_toolbar_click(x)
            return

        # Commit text if editing
        if self._text_editing:
            self._commit_text()
            return

        if self.tool == Tool.SELECT:
            self._begin_select(gesture, x, y)
        elif self.tool == Tool.TEXT:
            ix, iy = self._widget_to_image(x, y)
            self._text_pos = (ix, iy)
            self._text_input = ""
            self._text_editing = True
            self._drawing_area.set_cursor(self._cursor_ibeam)
            self._drawing_area.queue_draw()
        else:
            ix, iy = self._widget_to_image(x, y)
            self.current_ann = Annotation(
                tool=self.tool, color=self.color, x1=ix, y1=iy, x2=ix, y2=iy
            )

    def _on_drag_update(
        self, _gesture: Gtk.GestureDrag, offset_x: float, offset_y: float
    ) -> None:
        if self.tool == Tool.SELECT:
            self._update_select(offset_x, offset_y)
        elif self.current_ann is not None:
            alloc = self._drawing_area.get_allocation()
            scale, ox, oy = self._get_scale(alloc.width, alloc.height)
            wx1 = self.current_ann.x1 * scale + ox
            wy1 = self.current_ann.y1 * scale + oy
            self.current_ann.x2, self.current_ann.y2 = self._widget_to_image(
                wx1 + offset_x, wy1 + offset_y
            )
            self._drawing_area.queue_draw()

    def _on_drag_end(
        self, _gesture: Gtk.GestureDrag, offset_x: float, offset_y: float
    ) -> None:
        if self.tool == Tool.SELECT:
            self._end_select(offset_x, offset_y)
        elif self.current_ann is not None:
            dx = abs(self.current_ann.x2 - self.current_ann.x1)
            dy = abs(self.current_ann.y2 - self.current_ann.y1)
            if dx > 2 or dy > 2:
                self.annotations.append(self.current_ann)
            self.current_ann = None
            self._drawing_area.queue_draw()

    # -- select mode helpers --------------------------------------------------

    def _begin_select(self, gesture: Gtk.GestureDrag, x: float, y: float) -> None:
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

    def _update_select(self, offset_x: float, offset_y: float) -> None:
        if not self.dragging:
            return
        self.drag_current_x = self.drag_start_x + offset_x
        self.drag_current_y = self.drag_start_y + offset_y

        if (offset_x**2 + offset_y**2) ** 0.5 >= MIN_DRAG_DISTANCE:
            self._drag_exceeded_threshold = True

        if not self._drag_exceeded_threshold:
            return

        ix1, iy1 = self._widget_to_image(
            min(self.drag_start_x, self.drag_current_x),
            min(self.drag_start_y, self.drag_current_y),
        )
        ix2, iy2 = self._widget_to_image(
            max(self.drag_start_x, self.drag_current_x),
            max(self.drag_start_y, self.drag_current_y),
        )

        self.selected_words.clear()
        for li, line in enumerate(self.lines):
            for wi, word in enumerate(line.words):
                if word.intersects_rect(
                    int(ix1), int(iy1), int(ix2 - ix1), int(iy2 - iy1)
                ):
                    self.selected_words.add((li, wi))
        self._drawing_area.queue_draw()

    def _end_select(self, offset_x: float, offset_y: float) -> None:
        was_drag = self._drag_exceeded_threshold
        self.dragging = False
        self._drag_exceeded_threshold = False

        if was_drag:
            self._drawing_area.queue_draw()
            return

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

    # -- toolbar --------------------------------------------------------------

    def _handle_toolbar_click(self, x: float) -> None:
        bx = 10.0
        for tool in Tool:
            if bx <= x <= bx + TOOL_BTN_W:
                self._switch_tool(tool)
                return
            bx += TOOL_BTN_W + 6

        if self.tool != Tool.SELECT:
            cx = bx + 20
            for i in range(len(COLORS)):
                if cx <= x <= cx + COLOR_BTN_SIZE:
                    self.color_idx = i
                    self._drawing_area.queue_draw()
                    return
                cx += COLOR_BTN_SIZE + 4

    def _switch_tool(self, tool: Tool) -> None:
        self.tool = tool
        if tool == Tool.SELECT:
            self._drawing_area.set_cursor(self._cursor_default)
        elif tool == Tool.TEXT:
            self._drawing_area.set_cursor(self._cursor_ibeam)
        else:
            self._drawing_area.set_cursor(self._cursor_cross)
        self._drawing_area.queue_draw()

    def _commit_text(self) -> None:
        if self._text_input.strip():
            self.annotations.append(
                Annotation(
                    tool=Tool.TEXT,
                    color=self.color,
                    x1=self._text_pos[0],
                    y1=self._text_pos[1],
                    text=self._text_input,
                )
            )
        self._text_editing = False
        self._text_input = ""
        if self.tool == Tool.TEXT:
            self._drawing_area.set_cursor(self._cursor_ibeam)
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

        # Text annotation editing
        if self._text_editing:
            if keyval == Gdk.KEY_Escape:
                self._text_editing = False
                self._text_input = ""
                self._drawing_area.queue_draw()
                return True
            if keyval in (Gdk.KEY_Return, Gdk.KEY_KP_Enter):
                self._commit_text()
                return True
            if keyval == Gdk.KEY_BackSpace:
                self._text_input = self._text_input[:-1]
                self._drawing_area.queue_draw()
                return True
            char = Gdk.keyval_to_unicode(keyval)
            if char > 0:
                self._text_input += chr(char)
                self._drawing_area.queue_draw()
                return True
            return False

        if keyval == Gdk.KEY_Escape:
            self.app.quit()
            return True

        if ctrl and keyval == Gdk.KEY_c:
            if self.annotations:
                self._copy_annotated_image()
            else:
                self._copy_selected_text()
            return True

        if keyval in (Gdk.KEY_Return, Gdk.KEY_KP_Enter):
            if self.annotations:
                self._copy_annotated_image()
            else:
                self._copy_selected_text()
            return True

        if ctrl and keyval == Gdk.KEY_s:
            self._save_image()
            return True

        if ctrl and keyval == Gdk.KEY_z:
            if self.annotations:
                self.annotations.pop()
                self._drawing_area.queue_draw()
            return True

        if ctrl and keyval == Gdk.KEY_a:
            self._switch_tool(Tool.SELECT)
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

    def _copy_selected_text(self) -> None:
        if not self.selected_words:
            return
        line_texts: dict[int, list[str]] = {}
        for li, wi in sorted(self.selected_words):
            line_texts.setdefault(li, []).append(self.lines[li].words[wi].text)
        text = "\n".join(" ".join(words) for words in line_texts.values())
        try:
            subprocess.run([self.wl_copy_cmd], input=text.encode(), check=True)
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            print(f"live-text: clipboard error: {e}")
            return
        self._flashing = True
        self._drawing_area.queue_draw()
        GLib.timeout_add(FLASH_DURATION_MS, self._end_flash)

    def _render_image(self) -> cairo.ImageSurface | None:
        """Render the screenshot with annotations baked in at full resolution."""
        if self._image_surface is None:
            return None
        img_w = self._image_surface.get_width()
        img_h = self._image_surface.get_height()
        out = cairo.ImageSurface(cairo.FORMAT_ARGB32, img_w, img_h)
        cr = cairo.Context(out)
        cr.set_source_surface(self._image_surface, 0, 0)
        cr.paint()
        for ann in self.annotations:
            self._draw_annotation(cr, ann, 1.0)
        return out

    def _copy_annotated_image(self) -> None:
        out = self._render_image()
        if out is None:
            return

        with tempfile.NamedTemporaryFile(
            suffix=".png", prefix="annotated-", delete=False
        ) as tmp:
            out.write_to_png(tmp.name)
            tmp_path = Path(tmp.name)
        try:
            with open(tmp_path, "rb") as f:
                subprocess.run(
                    [self.wl_copy_cmd, "-t", "image/png"], stdin=f, check=True
                )
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            print(f"live-text: clipboard error: {e}")
        finally:
            tmp_path.unlink(missing_ok=True)

        self._flashing = True
        self._drawing_area.queue_draw()
        GLib.timeout_add(FLASH_DURATION_MS, self._end_flash)

    def _save_image(self) -> None:
        """Save the (annotated) screenshot to ~/Pictures/Screenshots/."""
        from datetime import datetime

        out = self._render_image()
        if out is None:
            return

        save_dir = Path.home() / "Pictures" / "Screenshots"
        save_dir.mkdir(parents=True, exist_ok=True)
        timestamp = datetime.now().strftime("%Y-%m-%d-%H-%M-%S")
        save_path = save_dir / f"screenshot-{timestamp}.png"
        out.write_to_png(str(save_path))

        print(f"Saved to {save_path}")
        try:
            subprocess.run(
                ["notify-send", "-t", "2000", "Live Text", f"Saved to {save_path}"],
                check=False,
            )
        except FileNotFoundError:
            pass

        self._flashing = True
        self._drawing_area.queue_draw()
        GLib.timeout_add(FLASH_DURATION_MS, self._end_flash)
