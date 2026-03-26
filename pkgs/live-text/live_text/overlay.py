"""GTK4 Layer Shell overlay: text selection + annotation.

Single unified overlay that combines macOS Live Text-style word selection
with screenshot annotation (arrows, rectangles, text).  The toolbar at
the bottom lets you switch between Select mode and drawing tools.
"""

from __future__ import annotations

import enum
import math
import re
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path

import cairo
import gi

gi.require_version("Gdk", "4.0")
gi.require_version("Gtk", "4.0")
gi.require_version("Gtk4LayerShell", "1.0")

from gi.repository import Gdk, Gio, GLib, Gtk, Gtk4LayerShell  # noqa: E402

from .barcode import CodeBox  # noqa: E402
from .ocr import LineBox, WordBox  # noqa: E402

# -- constants ----------------------------------------------------------------

# Text selection
SELECTION_FILL = (0.26, 0.52, 0.96, 0.35)
FLASH_FILL = (0.26, 0.52, 0.96, 0.6)
DRAG_RECT_COLOR = (0.26, 0.52, 0.96, 0.3)
DRAG_RECT_BORDER = (0.26, 0.52, 0.96, 0.7)
WORD_PADDING = 1
MIN_DRAG_DISTANCE = 5.0
FLASH_DURATION_MS = 150
TOAST_DURATION_MS = 900
SPINNER_FRAMES = "◐◓◑◒"
SPINNER_INTERVAL_MS = 120
DOUBLE_CLICK_MS = 400

# Hover highlight
HOVER_FILL = (0.26, 0.52, 0.96, 0.12)

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

# Context menu
MENU_BG = (0.18, 0.18, 0.18, 0.95)
MENU_HOVER_BG = (0.3, 0.5, 0.9, 0.9)
MENU_TEXT_COLOR = (1.0, 1.0, 1.0)
MENU_FONT_SIZE = 13.0
MENU_ITEM_H = 28.0
MENU_PADDING_X = 16.0
MENU_PADDING_Y = 6.0
MENU_MIN_W = 180.0
MENU_BORDER_RADIUS = 6.0
MENU_SEPARATOR_H = 9.0

# QR / barcode highlights
CODE_BORDER_COLOR = (0.0, 0.8, 0.4, 0.9)
CODE_FILL_COLOR = (0.0, 0.8, 0.4, 0.15)
CODE_SELECTED_FILL = (0.0, 0.8, 0.4, 0.4)
CODE_LABEL_FONT_SIZE = 11.0
CODE_LABEL_BG = (0.0, 0.0, 0.0, 0.7)
CODE_BORDER_WIDTH = 2.0

# Zoom / pan
ZOOM_MIN = 0.5
ZOOM_MAX = 10.0
ZOOM_STEP = 1.15  # multiplier per scroll tick

# URL / email detection.  Email local-part deliberately excludes common
# surrounding punctuation so "(foo@bar.com)" extracts the address only.
_URL_RE = re.compile(
    r"https?://[^\s<>\"']+"
    r"|www\.[^\s<>\"']+"
    r"|[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}"
)

# -- data types ---------------------------------------------------------------


class Tool(enum.Enum):
    SELECT = "Select"
    ARROW = "Arrow"
    RECT = "Rect"
    TEXT = "Text"


# Keyboard shortcuts for tools (1-4)
_TOOL_KEYS: dict[int, Tool] = {
    Gdk.KEY_1: Tool.SELECT,
    Gdk.KEY_2: Tool.ARROW,
    Gdk.KEY_3: Tool.RECT,
    Gdk.KEY_4: Tool.TEXT,
}


@dataclass
class Annotation:
    tool: Tool
    color: tuple[float, float, float]
    x1: float = 0.0
    y1: float = 0.0
    x2: float = 0.0
    y2: float = 0.0
    text: str = ""


@dataclass
class MenuItem:
    """An entry in the right-click context menu."""

    label: str
    action: str  # internal action key
    shortcut: str = ""  # hint text shown on the right


WordId = tuple[int, int]


def _word_rect(w: WordBox) -> tuple[float, float, float, float]:
    return (
        w.x - WORD_PADDING,
        w.y - WORD_PADDING,
        w.width + 2 * WORD_PADDING,
        w.height + 2 * WORD_PADDING,
    )


def _rounded_rect(
    cr: cairo.Context, x: float, y: float, w: float, h: float, r: float
) -> None:
    """Add a rounded rectangle sub-path to the cairo context."""
    cr.new_sub_path()
    cr.arc(x + w - r, y + r, r, -math.pi / 2, 0)
    cr.arc(x + w - r, y + h - r, r, 0, math.pi / 2)
    cr.arc(x + r, y + h - r, r, math.pi / 2, math.pi)
    cr.arc(x + r, y + r, r, math.pi, 3 * math.pi / 2)
    cr.close_path()


# -- overlay ------------------------------------------------------------------


class LiveTextOverlay:
    """Unified overlay: OCR text selection + annotation drawing."""

    def __init__(
        self,
        screenshot_path: Path,
        lines: list[LineBox],
        wl_copy_cmd: str = "wl-copy",
        codes: list[CodeBox] | None = None,
    ) -> None:
        self.screenshot_path = screenshot_path
        self.lines = lines
        self.wl_copy_cmd = wl_copy_cmd
        self.codes: list[CodeBox] = codes or []

        # Current tool
        self.tool = Tool.SELECT

        # Text selection state
        self.selected_words: set[WordId] = set()
        self.selected_codes: set[int] = set()  # indices into self.codes
        self._over_text = False
        self._flashing = False
        self._toast: str | None = None
        self._toast_timer: int | None = None
        self._flash_timer: int | None = None
        self._last_click_time: int = 0  # monotonic µs for double-click

        # Hover highlight: the word or code currently under the cursor
        self._hover_word: WordId | None = None
        self._hover_code: int | None = None

        # Annotation state
        self.color_idx = 0
        self.annotations: list[Annotation] = []
        self._redo_stack: list[Annotation] = []
        self.current_ann: Annotation | None = None
        self._text_input = ""
        self._text_pos: tuple[float, float] = (0.0, 0.0)
        self._text_editing = False

        # Spinner for OCR progress
        self._spinner_idx = 0
        self._spinner_timer: int | None = None

        # Error messages from background workers
        self._errors: list[str] = []

        # Drag state (used by both select and annotation)
        self.dragging = False
        self.drag_start_x = 0.0
        self.drag_start_y = 0.0
        self.drag_current_x = 0.0
        self.drag_current_y = 0.0
        self._drag_exceeded_threshold = False
        self._drag_shift = False
        self._drag_base_words: set[WordId] = set()
        self._drag_base_codes: set[int] = set()

        # Zoom & pan state
        self._zoom = 1.0  # additional zoom on top of fit-to-window scale
        self._pan_x = 0.0  # pan offset in widget pixels
        self._pan_y = 0.0
        self._panning = False
        self._pan_start_pan_x = 0.0
        self._pan_start_pan_y = 0.0

        # Context menu state
        self._menu_visible = False
        self._menu_x = 0.0  # widget coords where menu was opened
        self._menu_y = 0.0
        self._menu_items: list[MenuItem | None] = []  # None = separator
        self._menu_hover_idx: int = -1

        # Escape-to-quit confirmation when unsaved annotations exist
        self._escape_pending = False
        self._escape_timer: int | None = None

        # Tooltip for QR/barcode full data on hover
        self._tooltip_text: str | None = None
        self._tooltip_x = 0.0  # widget coords
        self._tooltip_y = 0.0

        # Last known cursor position (for scroll-to-zoom centering)
        self._cursor_x = 0.0
        self._cursor_y = 0.0

        self._image_surface: cairo.ImageSurface | None = None
        # Set once _on_activate has created the drawing area, so
        # background-worker callbacks that race ahead of GTK startup
        # don't touch a widget that doesn't exist yet.
        self._activated = False
        self._cursor_ibeam = Gdk.Cursor.new_from_name("text")
        self._cursor_cross = Gdk.Cursor.new_from_name("crosshair")
        self._cursor_default = Gdk.Cursor.new_from_name("default")
        self._cursor_grabbing = Gdk.Cursor.new_from_name("grabbing")

        # NON_UNIQUE skips D-Bus name ownership.  With OCR running in a
        # forked subprocess the synchronous GDBus registration can dead-
        # lock against the ProcessPoolExecutor manager thread and time
        # out after 25 s.  A layer-shell overlay has no use for single-
        # instance semantics anyway.
        self.app = Gtk.Application(
            application_id="org.mic92.live-text",
            flags=Gio.ApplicationFlags.NON_UNIQUE,
        )
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
        self.selected_words.clear()  # indices are invalidated by new list
        self._hover_word = None  # stale index would cause IndexError
        if self._spinner_timer is not None:
            GLib.source_remove(self._spinner_timer)
            self._spinner_timer = None
        if self._activated:
            self._drawing_area.queue_draw()
        return False  # don't repeat

    def set_codes(self, codes: list[CodeBox]) -> None:
        """Update detected codes from a background thread. Thread-safe."""
        GLib.idle_add(self._apply_codes, codes)

    def _apply_codes(self, codes: list[CodeBox]) -> bool:
        """GLib.idle callback: apply barcode results on the main thread."""
        self.codes = codes
        self.selected_codes.clear()  # indices are invalidated by new list
        self._hover_code = None  # stale index would cause IndexError
        self._tooltip_text = None
        if self._activated:
            self._drawing_area.queue_draw()
        return False

    def set_error(self, message: str) -> None:
        """Report an error from a background thread. Thread-safe."""
        GLib.idle_add(self._apply_error, message)

    def _apply_error(self, message: str) -> bool:
        """GLib.idle callback: store error and redraw."""
        self._errors.append(message)
        if self._activated:
            self._drawing_area.queue_draw()
        return False

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

        # Middle-click drag for panning
        pan_drag = Gtk.GestureDrag(button=2)
        pan_drag.connect("drag-begin", self._on_pan_begin)
        pan_drag.connect("drag-update", self._on_pan_update)
        pan_drag.connect("drag-end", self._on_pan_end)
        da.add_controller(pan_drag)

        # Right-click for context menu
        right_click = Gtk.GestureClick(button=3)
        right_click.connect("released", self._on_right_click)
        da.add_controller(right_click)

        # Scroll for zoom
        scroll = Gtk.EventControllerScroll(
            flags=Gtk.EventControllerScrollFlags.VERTICAL
        )
        scroll.connect("scroll", self._on_scroll)
        da.add_controller(scroll)

        keys = Gtk.EventControllerKey()
        keys.connect("key-pressed", self._on_key_pressed)
        window.add_controller(keys)

        da.set_cursor(self._cursor_default)
        window.set_child(da)
        window.present()
        self._activated = True

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

    def _get_base_scale(
        self, widget_w: int | None = None, widget_h: int | None = None
    ) -> tuple[float, float, float]:
        """Return (base_scale, center_ox, center_oy) ignoring zoom/pan.

        Used internally by _get_scale and for zoom calculations.
        """
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

    def _get_scale(
        self, widget_w: int | None = None, widget_h: int | None = None
    ) -> tuple[float, float, float]:
        """Return (effective_scale, offset_x, offset_y) with zoom and pan."""
        base_scale, base_ox, base_oy = self._get_base_scale(widget_w, widget_h)
        scale = base_scale * self._zoom
        # When zoom > 1, keep the center of the viewport fixed, plus pan offset
        ox = base_ox * self._zoom + self._pan_x
        oy = base_oy * self._zoom + self._pan_y
        return scale, ox, oy

    def _widget_to_image(self, wx: float, wy: float) -> tuple[float, float]:
        scale, ox, oy = self._get_scale()
        return (wx - ox) / scale, (wy - oy) / scale

    def _hit_word(self, ix: float, iy: float) -> WordId | None:
        for li, line in enumerate(self.lines):
            for wi, word in enumerate(line.words):
                if word.contains_point(ix, iy):
                    return (li, wi)
        return None

    def _hit_code(self, ix: float, iy: float) -> int | None:
        """Return the index of the code at image coords, or None."""
        for ci, code in enumerate(self.codes):
            if code.contains_point(ix, iy):
                return ci
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

        # Hover highlight (subtle tint on the word under the cursor)
        if (
            self._hover_word is not None
            and self._hover_word not in self.selected_words
            and not self.dragging
        ):
            li, wi = self._hover_word
            cr.set_source_rgba(*HOVER_FILL)
            cr.rectangle(*_word_rect(self.lines[li].words[wi]))
            cr.fill()

        # Text selection highlight
        if self.selected_words:
            fill = FLASH_FILL if self._flashing else SELECTION_FILL
            cr.set_source_rgba(*fill)
            for li, wi in self.selected_words:
                cr.rectangle(*_word_rect(self.lines[li].words[wi]))
            cr.fill()

        # QR / barcode overlays
        for ci, code in enumerate(self.codes):
            is_selected = ci in self.selected_codes
            is_hovered = ci == self._hover_code and not is_selected
            # Fill
            if is_selected and self._flashing:
                cr.set_source_rgba(0.0, 0.8, 0.4, 0.6)
            elif is_selected:
                cr.set_source_rgba(*CODE_SELECTED_FILL)
            elif is_hovered:
                cr.set_source_rgba(0.0, 0.8, 0.4, 0.25)
            else:
                cr.set_source_rgba(*CODE_FILL_COLOR)
            cr.rectangle(code.x, code.y, code.width, code.height)
            cr.fill()
            # Border
            cr.set_source_rgba(*CODE_BORDER_COLOR)
            cr.set_line_width(CODE_BORDER_WIDTH / scale)
            cr.rectangle(code.x, code.y, code.width, code.height)
            cr.stroke()
            # Label (type + truncated data).  Counter-scale the font so
            # it stays readable at any zoom level instead of becoming
            # microscopic at <1× or screen-filling at 10×.
            label = code.code_type
            preview = code.data[:40] + ("…" if len(code.data) > 40 else "")
            label_text = f"{label}: {preview}"
            cr.set_font_size(CODE_LABEL_FONT_SIZE / scale)
            ext = cr.text_extents(label_text)
            lx = code.x
            ly = code.y - 4 / scale  # above the box
            pad = 2 / scale
            cr.set_source_rgba(*CODE_LABEL_BG)
            cr.rectangle(
                lx - pad,
                ly - ext.height - pad,
                ext.width + 3 * pad,
                ext.height + 2 * pad,
            )
            cr.fill()
            cr.set_source_rgba(0.0, 1.0, 0.5, 1.0)
            cr.move_to(lx, ly)
            cr.show_text(label_text)

        # Drag selection rectangle (in image coords)
        if self.tool == Tool.SELECT and self.dragging and self._drag_exceeded_threshold:
            ix1, iy1 = self._widget_to_image(
                min(self.drag_start_x, self.drag_current_x),
                min(self.drag_start_y, self.drag_current_y),
            )
            ix2, iy2 = self._widget_to_image(
                max(self.drag_start_x, self.drag_current_x),
                max(self.drag_start_y, self.drag_current_y),
            )
            rw, rh = ix2 - ix1, iy2 - iy1
            # Fill
            cr.set_source_rgba(*DRAG_RECT_COLOR)
            cr.rectangle(ix1, iy1, rw, rh)
            cr.fill()
            # Dashed border
            cr.set_source_rgba(*DRAG_RECT_BORDER)
            cr.set_line_width(1.5 / scale)
            cr.set_dash([6.0 / scale, 4.0 / scale])
            cr.rectangle(ix1, iy1, rw, rh)
            cr.stroke()
            cr.set_dash([])

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
        self._draw_tooltip(cr, width, height)
        self._draw_context_menu(cr, width, height)

    def _draw_annotation(
        self, cr: cairo.Context, ann: Annotation, scale: float
    ) -> None:
        cr.set_source_rgb(*ann.color)
        cr.set_line_width(STROKE_WIDTH / scale)

        if ann.tool == Tool.ARROW:
            cr.move_to(ann.x1, ann.y1)
            cr.line_to(ann.x2, ann.y2)
            cr.stroke()
            # Only draw arrowhead if the line has nonzero length
            adx = ann.x2 - ann.x1
            ady = ann.y2 - ann.y1
            if adx != 0 or ady != 0:
                angle = math.atan2(ady, adx)
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
        hint = self._build_hint()
        cr.set_source_rgba(1, 1, 1, 0.6)
        cr.set_font_size(HINT_FONT_SIZE)
        ext = cr.text_extents(hint)
        cr.move_to(width - ext.width - 16, bar_y + TOOLBAR_H / 2 + ext.height / 2)
        cr.show_text(hint)

    def _build_hint(self) -> str:
        """Build context-sensitive hint text for the toolbar."""
        if self._toast:
            return self._toast

        if self._escape_pending:
            return "⚠ Unsaved annotations — press Esc again to quit"

        if self._errors:
            first_line = self._errors[0].split("\n")[0]
            return f"⚠ {first_line[:80]}  · Ctrl+S save · Esc quit"

        zoom_str = ""
        if self._zoom != 1.0:
            zoom_str = f" · {self._zoom:.0%}"

        if self.tool == Tool.SELECT:
            if not self.lines and self._spinner_timer is not None:
                frame = SPINNER_FRAMES[self._spinner_idx]
                return f"{frame} Detecting text…  · Ctrl+S save · Esc quit{zoom_str}"
            if not self.lines and not self.codes:
                return f"No text detected  · Ctrl+S save · Esc quit{zoom_str}"

            if self._has_selection():
                return (
                    f"Enter copy+close · Ctrl+C copy · Ctrl+A all · Esc quit{zoom_str}"
                )
            return f"Ctrl+C copy · Ctrl+S save · Ctrl+A all · Esc quit{zoom_str}"

        return f"Ctrl+C copy image · Ctrl+S save · Ctrl+Z undo · Esc quit{zoom_str}"

    def _draw_tooltip(self, cr: cairo.Context, width: int, height: int) -> None:
        """Draw a tooltip showing the full QR/barcode data near the cursor."""
        if self._tooltip_text is None:
            return

        cr.set_font_size(MENU_FONT_SIZE)
        # Wrap long text into multiple lines
        max_chars = 60
        text = self._tooltip_text
        lines: list[str] = []
        while text:
            lines.append(text[:max_chars])
            text = text[max_chars:]

        line_height = MENU_FONT_SIZE + 4
        max_w = 0.0
        for line in lines:
            ext = cr.text_extents(line)
            max_w = max(max_w, ext.width)

        pad = 8.0
        tw = max_w + pad * 2
        th = len(lines) * line_height + pad * 2

        # Position: below and to the right of cursor, clamped to viewport
        tx = min(self._tooltip_x + 12, width - tw - 4)
        ty = min(self._tooltip_y + 20, height - TOOLBAR_H - th - 4)
        tx = max(tx, 4)
        ty = max(ty, 4)

        # Background
        cr.set_source_rgba(0.0, 0.0, 0.0, 0.85)
        cr.rectangle(tx, ty, tw, th)
        cr.fill()
        # Border
        cr.set_source_rgba(0.0, 0.8, 0.4, 0.6)
        cr.set_line_width(1)
        cr.rectangle(tx, ty, tw, th)
        cr.stroke()
        # Text
        cr.set_source_rgba(0.9, 1.0, 0.9, 1.0)
        for i, line in enumerate(lines):
            cr.move_to(tx + pad, ty + pad + (i + 1) * line_height - 4)
            cr.show_text(line)

    def _menu_layout(
        self, cr: cairo.Context, width: int, height: int
    ) -> tuple[float, float, float, float]:
        """Calculate menu position and size. Returns (mx, my, menu_w, menu_h)."""
        cr.set_font_size(MENU_FONT_SIZE)

        max_label_w = 0.0
        max_shortcut_w = 0.0
        for item in self._menu_items:
            if item is not None:
                ext = cr.text_extents(item.label)
                max_label_w = max(max_label_w, ext.width)
                if item.shortcut:
                    ext2 = cr.text_extents(item.shortcut)
                    max_shortcut_w = max(max_shortcut_w, ext2.width)

        gap = 24.0 if max_shortcut_w > 0 else 0.0
        menu_w = max(
            MENU_MIN_W,
            max_label_w + max_shortcut_w + gap + MENU_PADDING_X * 2,
        )
        menu_h = MENU_PADDING_Y * 2
        for item in self._menu_items:
            menu_h += MENU_SEPARATOR_H if item is None else MENU_ITEM_H

        mx = min(self._menu_x, width - menu_w - 4)
        my = min(self._menu_y, height - TOOLBAR_H - menu_h - 4)
        mx = max(mx, 4)
        my = max(my, 4)
        return mx, my, menu_w, menu_h

    def _draw_context_menu(self, cr: cairo.Context, width: int, height: int) -> None:
        """Draw the right-click context menu if visible."""
        if not self._menu_visible or not self._menu_items:
            return

        mx, my, menu_w, menu_h = self._menu_layout(cr, width, height)

        # Background with rounded corners
        _rounded_rect(cr, mx, my, menu_w, menu_h, MENU_BORDER_RADIUS)
        cr.set_source_rgba(*MENU_BG)
        cr.fill()

        # Items
        iy = my + MENU_PADDING_Y
        for idx, item in enumerate(self._menu_items):
            if item is None:
                sep_y = iy + MENU_SEPARATOR_H / 2
                cr.set_source_rgba(1, 1, 1, 0.15)
                cr.move_to(mx + MENU_PADDING_X, sep_y)
                cr.line_to(mx + menu_w - MENU_PADDING_X, sep_y)
                cr.set_line_width(1)
                cr.stroke()
                iy += MENU_SEPARATOR_H
                continue

            if idx == self._menu_hover_idx:
                cr.set_source_rgba(*MENU_HOVER_BG)
                _rounded_rect(cr, mx + 4, iy, menu_w - 8, MENU_ITEM_H, 4)
                cr.fill()

            cr.set_source_rgb(*MENU_TEXT_COLOR)
            cr.set_font_size(MENU_FONT_SIZE)
            ext = cr.text_extents(item.label)
            cr.move_to(
                mx + MENU_PADDING_X,
                iy + MENU_ITEM_H / 2 + ext.height / 2,
            )
            cr.show_text(item.label)

            if item.shortcut:
                cr.set_source_rgba(1, 1, 1, 0.45)
                ext2 = cr.text_extents(item.shortcut)
                cr.move_to(
                    mx + menu_w - MENU_PADDING_X - ext2.width,
                    iy + MENU_ITEM_H / 2 + ext2.height / 2,
                )
                cr.show_text(item.shortcut)

            iy += MENU_ITEM_H

    def _hit_menu_item(self, x: float, y: float) -> int:
        """Return the index of the menu item under (x, y), or -1."""
        if not self._menu_visible or not self._menu_items:
            return -1

        alloc = self._drawing_area.get_allocation()
        tmp = cairo.ImageSurface(cairo.FORMAT_ARGB32, 1, 1)
        cr = cairo.Context(tmp)
        mx, my, menu_w, menu_h = self._menu_layout(cr, alloc.width, alloc.height)

        if x < mx or x > mx + menu_w or y < my or y > my + menu_h:
            return -1

        iy = my + MENU_PADDING_Y
        for idx, item in enumerate(self._menu_items):
            if item is None:
                iy += MENU_SEPARATOR_H
                continue
            if iy <= y <= iy + MENU_ITEM_H:
                return idx
            iy += MENU_ITEM_H
        return -1

    # -- mouse ----------------------------------------------------------------

    def _on_motion(
        self, _controller: Gtk.EventControllerMotion, x: float, y: float
    ) -> None:
        self._cursor_x = x
        self._cursor_y = y

        # Update context menu hover
        if self._menu_visible:
            self._menu_hover_idx = self._hit_menu_item(x, y)
            self._drawing_area.queue_draw()
            return

        if self.tool != Tool.SELECT:
            return
        ix, iy = self._widget_to_image(x, y)

        # Hover highlight for words
        new_hover_word = self._hit_word(ix, iy)
        new_hover_code = self._hit_code(ix, iy)
        hover_changed = (
            new_hover_word != self._hover_word or new_hover_code != self._hover_code
        )
        self._hover_word = new_hover_word
        self._hover_code = new_hover_code

        over = new_hover_word is not None or new_hover_code is not None
        if over != self._over_text:
            self._over_text = over
            self._drawing_area.set_cursor(
                self._cursor_ibeam if over else self._cursor_default
            )

        # Tooltip for QR/barcode full data
        old_tooltip = self._tooltip_text
        if new_hover_code is not None:
            code = self.codes[new_hover_code]
            if len(code.data) > 40:
                self._tooltip_text = code.data
                self._tooltip_x = x
                self._tooltip_y = y
            else:
                self._tooltip_text = None
        else:
            self._tooltip_text = None

        if hover_changed or self._tooltip_text != old_tooltip:
            self._drawing_area.queue_draw()

    def _on_drag_begin(self, gesture: Gtk.GestureDrag, x: float, y: float) -> None:
        # Dismiss context menu on any click
        if self._menu_visible:
            idx = self._hit_menu_item(x, y)
            self._menu_visible = False
            self._drawing_area.queue_draw()
            if idx >= 0:
                # Click was on a menu item — execute it and consume the click
                item = self._menu_items[idx]
                if item is not None:
                    self._execute_menu_action(item.action)
                return
            # Click was outside menu — fall through so the click is not lost

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
            # Starting a new annotation clears the redo stack
            self._redo_stack.clear()
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

        # Read Shift state from the gesture's modifier mask.  Previously
        # this queried seat.get_keyboard() which returns None on some
        # seats (e.g. tablet-only) and crashed with AttributeError.
        state = gesture.get_current_event_state()
        self._drag_shift = bool(state & Gdk.ModifierType.SHIFT_MASK)

        # Snapshot the selection so Shift+drag can extend it instead of
        # replacing it on every mouse-move.
        self._drag_base_words = set(self.selected_words)
        self._drag_base_codes = set(self.selected_codes)

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

        rect = (int(ix1), int(iy1), int(ix2 - ix1), int(iy2 - iy1))
        hit_words: set[WordId] = set()
        hit_codes: set[int] = set()
        for li, line in enumerate(self.lines):
            for wi, word in enumerate(line.words):
                if word.intersects_rect(*rect):
                    hit_words.add((li, wi))
        for ci, code in enumerate(self.codes):
            if code.intersects_rect(*rect):
                hit_codes.add(ci)

        if self._drag_shift:
            # Extend the selection that existed when the drag started
            self.selected_words = self._drag_base_words | hit_words
            self.selected_codes = self._drag_base_codes | hit_codes
        else:
            self.selected_words = hit_words
            self.selected_codes = hit_codes
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
        clicked_word = self._hit_word(ix, iy)
        clicked_code = self._hit_code(ix, iy)

        # Double-click detection
        now = GLib.get_monotonic_time()
        is_double = (now - self._last_click_time) < DOUBLE_CLICK_MS * 1000
        self._last_click_time = now

        if clicked_word is not None:
            li, _wi = clicked_word
            if is_double:
                # Select entire line
                all_words = {(li, wi) for wi in range(len(self.lines[li].words))}
                if self._drag_shift:
                    self.selected_words |= all_words
                else:
                    self.selected_words = all_words
                    self.selected_codes.clear()
            elif self._drag_shift:
                self.selected_words.symmetric_difference_update({clicked_word})
            else:
                self.selected_words = {clicked_word}
                self.selected_codes.clear()
        elif clicked_code is not None:
            if self._drag_shift:
                self.selected_codes.symmetric_difference_update({clicked_code})
            else:
                self.selected_codes = {clicked_code}
                self.selected_words.clear()
        else:
            self.selected_words.clear()
            self.selected_codes.clear()
        self._drawing_area.queue_draw()

    # -- zoom & pan -----------------------------------------------------------

    def _on_scroll(
        self,
        _controller: Gtk.EventControllerScroll,
        _dx: float,
        dy: float,
    ) -> bool:
        """Zoom in/out centered on the cursor position."""
        if self._menu_visible:
            return False

        wx = self._cursor_x
        wy = self._cursor_y

        old_zoom = self._zoom
        if dy < 0:
            self._zoom = min(self._zoom * ZOOM_STEP, ZOOM_MAX)
        else:
            self._zoom = max(self._zoom / ZOOM_STEP, ZOOM_MIN)

        # Adjust pan so the point under the cursor stays fixed
        zoom_ratio = self._zoom / old_zoom
        self._pan_x = wx - zoom_ratio * (wx - self._pan_x)
        self._pan_y = wy - zoom_ratio * (wy - self._pan_y)

        self._drawing_area.queue_draw()
        return True

    def _on_pan_begin(self, _gesture: Gtk.GestureDrag, _x: float, _y: float) -> None:
        """Start middle-click panning."""
        self._panning = True
        self._pan_start_pan_x = self._pan_x
        self._pan_start_pan_y = self._pan_y
        self._drawing_area.set_cursor(self._cursor_grabbing)

    def _on_pan_update(
        self, _gesture: Gtk.GestureDrag, offset_x: float, offset_y: float
    ) -> None:
        """Update pan offset during middle-click drag."""
        if not self._panning:
            return
        self._pan_x = self._pan_start_pan_x + offset_x
        self._pan_y = self._pan_start_pan_y + offset_y
        self._drawing_area.queue_draw()

    def _on_pan_end(
        self, _gesture: Gtk.GestureDrag, _offset_x: float, _offset_y: float
    ) -> None:
        """End middle-click panning."""
        self._panning = False
        # Restore appropriate cursor for current tool
        if self.tool == Tool.SELECT:
            self._drawing_area.set_cursor(
                self._cursor_ibeam if self._over_text else self._cursor_default
            )
        elif self.tool == Tool.TEXT:
            self._drawing_area.set_cursor(self._cursor_ibeam)
        else:
            self._drawing_area.set_cursor(self._cursor_cross)

    def _reset_zoom(self) -> None:
        """Reset zoom to 1x and clear pan offset."""
        self._zoom = 1.0
        self._pan_x = 0.0
        self._pan_y = 0.0
        self._drawing_area.queue_draw()

    # -- right-click context menu ---------------------------------------------

    def _on_right_click(
        self,
        _gesture: Gtk.GestureClick,
        _n_press: int,
        x: float,
        y: float,
    ) -> None:
        """Show context menu at click position."""
        alloc = self._drawing_area.get_allocation()
        if y >= alloc.height - TOOLBAR_H:
            return  # Don't open menu on toolbar

        self._menu_x = x
        self._menu_y = y
        self._menu_items = self._build_menu_items(x, y)
        self._menu_hover_idx = -1
        self._menu_visible = True
        self._drawing_area.queue_draw()

    def _build_menu_items(self, x: float, y: float) -> list[MenuItem | None]:
        """Build context menu items based on current state."""
        items: list[MenuItem | None] = []

        has_sel = self._has_selection()

        if has_sel:
            items.append(MenuItem("Copy text", "copy_text", "Ctrl+C"))
            items.append(MenuItem("Copy text & close", "copy_text_quit", "Enter"))

        items.append(
            MenuItem("Copy image", "copy_image", "Ctrl+C" if not has_sel else "")
        )
        items.append(MenuItem("Save image…", "save_image", "Ctrl+S"))

        if has_sel or self.lines or self.codes:
            items.append(None)  # separator

        if self.lines or self.codes:
            items.append(MenuItem("Select all", "select_all", "Ctrl+A"))

        # Check if the right-clicked point is over a specific line
        ix, iy = self._widget_to_image(x, y)
        clicked_word = self._hit_word(ix, iy)
        if clicked_word is not None:
            li, _wi = clicked_word
            items.append(
                MenuItem(
                    f"Select line {li + 1}",
                    f"select_line:{li}",
                    "Dbl-click",
                )
            )

        # Check if selected text or clicked code looks like a URL
        url = self._detect_url(x, y)
        if url:
            truncated = url[:40] + ("…" if len(url) > 40 else "")
            items.append(None)  # separator
            items.append(MenuItem(f"Open {truncated}", "open_url", ""))

        if self.annotations:
            items.append(None)  # separator
            items.append(MenuItem("Undo", "undo", "Ctrl+Z"))
            if self._redo_stack:
                items.append(MenuItem("Redo", "redo", "Ctrl+Shift+Z"))

        if self._zoom != 1.0:
            items.append(None)
            items.append(MenuItem("Reset zoom", "reset_zoom", "Ctrl+0"))

        return items

    def _detect_url(self, x: float, y: float) -> str | None:
        """Check if selection or right-clicked item contains a URL."""
        # Check selected text first
        word_text = self._selected_text()
        if word_text:
            match = _URL_RE.search(word_text)
            if match:
                return match.group(0)

        # Check selected codes
        for ci in self.selected_codes:
            match = _URL_RE.search(self.codes[ci].data)
            if match:
                return match.group(0)

        # Check code under cursor
        ix, iy = self._widget_to_image(x, y)
        hit_code = self._hit_code(ix, iy)
        if hit_code is not None:
            match = _URL_RE.search(self.codes[hit_code].data)
            if match:
                return match.group(0)

        return None

    def _execute_menu_action(self, action: str) -> None:
        """Execute a context menu action."""
        if action == "copy_text":
            self._copy_selected_text()
        elif action == "copy_text_quit":
            self._copy_selected_text()
            self.app.quit()
        elif action == "copy_image":
            self._copy_annotated_image()
        elif action == "save_image":
            self._save_image()
        elif action == "select_all":
            self._select_all()
        elif action.startswith("select_line:"):
            li = int(action.split(":")[1])
            self._switch_tool(Tool.SELECT)
            self.selected_words = {(li, wi) for wi in range(len(self.lines[li].words))}
            self._drawing_area.queue_draw()
        elif action == "open_url":
            url = self._detect_url(self._menu_x, self._menu_y)
            if url:
                # Add protocol if missing
                if not url.startswith(("http://", "https://", "mailto:")):
                    if "@" in url:
                        url = f"mailto:{url}"
                    else:
                        url = f"https://{url}"
                try:
                    subprocess.Popen(
                        ["xdg-open", url],
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                    )
                except FileNotFoundError:
                    self._show_toast("⚠ xdg-open not found")
        elif action == "undo":
            if self.annotations:
                self._redo_stack.append(self.annotations.pop())
                self._drawing_area.queue_draw()
        elif action == "redo":
            if self._redo_stack:
                self.annotations.append(self._redo_stack.pop())
                self._drawing_area.queue_draw()
        elif action == "reset_zoom":
            self._reset_zoom()

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
        # Clear hover state so stale highlights don't appear when switching back
        self._hover_word = None
        self._hover_code = None
        self._tooltip_text = None
        self._over_text = False
        if tool == Tool.SELECT:
            self._drawing_area.set_cursor(self._cursor_default)
        elif tool == Tool.TEXT:
            self._drawing_area.set_cursor(self._cursor_ibeam)
        else:
            self._drawing_area.set_cursor(self._cursor_cross)
        self._drawing_area.queue_draw()

    def _commit_text(self) -> None:
        if self._text_input.strip():
            self._redo_stack.clear()
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
        shift = bool(state & Gdk.ModifierType.SHIFT_MASK)

        # Dismiss context menu on any key press
        if self._menu_visible:
            self._menu_visible = False
            self._drawing_area.queue_draw()
            return True

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
            # If we have unsaved annotations, require double-Escape to quit
            if self.annotations and not self._escape_pending:
                self._escape_pending = True
                self._drawing_area.queue_draw()
                # Auto-clear the pending state after a timeout
                self._escape_timer = GLib.timeout_add(2000, self._clear_escape_pending)
                return True
            self.app.quit()
            return True

        # Any key other than Escape clears the pending escape state
        if self._escape_pending:
            self._escape_pending = False
            if self._escape_timer is not None:
                GLib.source_remove(self._escape_timer)
                self._escape_timer = None
            self._drawing_area.queue_draw()

        if ctrl and keyval == Gdk.KEY_c:
            if self._has_selection():
                self._copy_selected_text()
            else:
                self._copy_annotated_image()
            return True

        if keyval in (Gdk.KEY_Return, Gdk.KEY_KP_Enter):
            if self._has_selection():
                self._copy_selected_text()
                self.app.quit()
            else:
                self._copy_annotated_image()
            return True

        if ctrl and keyval == Gdk.KEY_s:
            self._save_image()
            return True

        # Redo: Ctrl+Shift+Z or Ctrl+Y
        if ctrl and shift and keyval == Gdk.KEY_Z:
            if self._redo_stack:
                self.annotations.append(self._redo_stack.pop())
                self._drawing_area.queue_draw()
            return True

        if ctrl and keyval == Gdk.KEY_y:
            if self._redo_stack:
                self.annotations.append(self._redo_stack.pop())
                self._drawing_area.queue_draw()
            return True

        # Undo: Ctrl+Z (without Shift)
        if ctrl and keyval == Gdk.KEY_z:
            if self.annotations:
                self._redo_stack.append(self.annotations.pop())
                self._drawing_area.queue_draw()
            return True

        # Reset zoom: Ctrl+0
        if ctrl and keyval == Gdk.KEY_0:
            self._reset_zoom()
            return True

        if ctrl and keyval == Gdk.KEY_a:
            self._select_all()
            return True

        # Tool shortcuts: 1-4 (only without ctrl)
        if not ctrl:
            tool = _TOOL_KEYS.get(keyval)
            if tool is not None:
                self._switch_tool(tool)
                return True

        return False

    def _clear_escape_pending(self) -> bool:
        """Clear the escape-to-quit confirmation after timeout."""
        self._escape_pending = False
        self._escape_timer = None
        self._drawing_area.queue_draw()
        return False

    # -- clipboard ------------------------------------------------------------

    def _end_flash(self) -> bool:
        self._flashing = False
        self._flash_timer = None
        self._drawing_area.queue_draw()
        return False

    def _show_toast(self, message: str) -> None:
        """Show a brief message in the toolbar hint area."""
        if self._toast_timer is not None:
            GLib.source_remove(self._toast_timer)
        self._toast = message
        self._drawing_area.queue_draw()
        self._toast_timer = GLib.timeout_add(TOAST_DURATION_MS, self._end_toast)

    def _end_toast(self) -> bool:
        self._toast = None
        self._toast_timer = None
        self._drawing_area.queue_draw()
        return False

    def _has_selection(self) -> bool:
        return bool(self.selected_words) or bool(self.selected_codes)

    def _select_all(self) -> None:
        """Select all detected words and codes."""
        self._switch_tool(Tool.SELECT)
        self.selected_words = {
            (li, wi)
            for li, line in enumerate(self.lines)
            for wi in range(len(line.words))
        }
        self.selected_codes = set(range(len(self.codes)))
        self._drawing_area.queue_draw()

    def _selected_text(self) -> str:
        """Assemble the selected words into a string with line breaks."""
        if not self.selected_words:
            return ""
        line_texts: dict[int, list[str]] = {}
        for li, wi in sorted(self.selected_words):
            line_texts.setdefault(li, []).append(self.lines[li].words[wi].text)
        return "\n".join(" ".join(words) for words in line_texts.values())

    def _copy_selected_text(self) -> None:
        if not self._has_selection():
            return
        parts: list[str] = []
        word_text = self._selected_text()
        if word_text:
            parts.append(word_text)
        for ci in sorted(self.selected_codes):
            parts.append(self.codes[ci].data)
        text = "\n".join(parts)
        try:
            subprocess.run([self.wl_copy_cmd], input=text.encode(), check=True)
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            print(f"live-text: clipboard error: {e}")
            return
        if self._flash_timer is not None:
            GLib.source_remove(self._flash_timer)
        self._flashing = True
        self._show_toast("✓ Copied text")
        self._drawing_area.queue_draw()
        self._flash_timer = GLib.timeout_add(FLASH_DURATION_MS, self._end_flash)

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
            return
        finally:
            tmp_path.unlink(missing_ok=True)

        self._show_toast("✓ Copied image")

    def _save_image(self) -> None:
        """Save the (annotated) screenshot to ~/Pictures/Screenshots/.

        Copies the saved path to the clipboard so it can be pasted into
        file managers, chat apps, etc.
        """
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

        # Copy the file path to clipboard for easy pasting
        try:
            subprocess.run(
                [self.wl_copy_cmd],
                input=str(save_path).encode(),
                check=True,
            )
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            print(f"live-text: clipboard error: {e}")

        try:
            subprocess.run(
                ["notify-send", "-t", "2000", "Live Text", f"Saved to {save_path}"],
                check=False,
            )
        except FileNotFoundError:
            pass

        self._show_toast(f"✓ Saved to {save_path.name}")
