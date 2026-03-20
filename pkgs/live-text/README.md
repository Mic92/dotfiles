# live-text

Interactive OCR overlay for Wayland (niri, sway, etc.) — the Linux equivalent of
macOS Live Text.

Takes a screenshot (or reads an image from stdin/file), runs OCR using RapidOCR
(PaddleOCR v4 via ONNX Runtime) and scans for QR codes/barcodes using pyzbar
(zbar), then shows an interactive overlay where you can select text or codes,
annotate with arrows/rectangles/text, and copy or save the result. OCR and
barcode scanning run in parallel so the overlay appears instantly.

## Usage

```bash
# Capture full screen and show overlay
live-text

# Capture a specific output
live-text --output eDP-1

# Select a region first, then OCR just that region
live-text --region

# Pick a window, then OCR just that window
live-text --window

# OCR an existing image file
live-text screenshot.png

# Pipeline mode: read from stdin
grim - | live-text -
```

## Overlay Modes

The toolbar at the bottom lets you switch between four tools:

- **Select** — Click or drag to select OCR-detected words or QR/barcodes
- **Arrow** — Draw arrows on the screenshot
- **Rect** — Draw rectangles on the screenshot
- **Text** — Click to place text annotations

In annotation modes (Arrow, Rect, Text), a color palette is shown in the toolbar
for picking the drawing color.

## Keybindings (in overlay)

### Text Selection (Select mode)

| Key              | Action                                  |
| ---------------- | --------------------------------------- |
| Click word       | Select it (replaces previous selection) |
| Shift+click      | Toggle word in selection                |
| Drag             | Select all words in swept area          |
| Double-click     | Select entire line                      |
| Click empty area | Clear selection                         |

### Annotation (Arrow / Rect / Text modes)

| Key       | Action                             |
| --------- | ---------------------------------- |
| Drag      | Draw arrow or rectangle            |
| Click     | Place text cursor (Text mode)      |
| Enter     | Commit typed text                  |
| Backspace | Delete last character while typing |
| Escape    | Cancel current text input          |

### Zoom & Pan

| Key                 | Action                          |
| ------------------- | ------------------------------- |
| Scroll wheel        | Zoom in/out (centered on image) |
| Middle-click + drag | Pan the image                   |
| Ctrl+0              | Reset zoom to fit               |

### Context Menu

Right-click anywhere on the image to open a context menu with:

- Copy text / Copy text & close (when text is selected)
- Copy image / Save image
- Select all / Select line
- Open URL (when selected text or QR code contains a link)
- Undo / Redo (when annotations exist)
- Reset zoom (when zoomed in)

### Global

| Key          | Action                                               |
| ------------ | ---------------------------------------------------- |
| Ctrl+C       | Copy selected text/codes, or annotated image if none |
| Enter        | Copy selected text/codes, or annotated image if none |
| Ctrl+A       | Select all detected words and codes                  |
| Ctrl+S       | Save annotated screenshot to ~/Pictures/Screenshots/ |
| Ctrl+Z       | Undo last annotation                                 |
| Ctrl+Shift+Z | Redo last undone annotation                          |
| Ctrl+Y       | Redo last undone annotation                          |
| Escape       | Quit (confirms first if unsaved annotations exist)   |

## Niri keybindings

| Key             | Action                                  |
| --------------- | --------------------------------------- |
| Mod+Shift+T     | Live Text: full screen OCR              |
| Mod+Ctrl+T      | Live Text: region OCR                   |
| Print           | Screenshot: region (niri built-in)      |
| Ctrl+Print      | Screenshot: full screen (niri built-in) |
| Alt+Print       | Screenshot: window (niri built-in)      |
| Mod+Print       | Screenshot: annotate region (Satty)     |
| Mod+Shift+Print | Screenshot: annotate screen (Satty)     |
| Mod+Ctrl+Print  | Screenshot: region → clipboard          |

## Architecture

```
live_text/
├── barcode.py     # pyzbar (zbar) QR code and barcode scanner
├── main.py        # CLI entry point, argument parsing, screenshot dispatch
├── ocr.py         # RapidOCR wrapper, word-level bounding box splitting
├── overlay.py     # GTK4 Layer Shell overlay (selection + annotation + toolbar)
└── screenshot.py  # grim/slurp integration, niri/sway output detection
```

OCR and barcode scanning run in parallel background threads — the overlay shows
a spinner until text detection completes, then word boxes and detected codes
become clickable.

## Dependencies

- `grim` — Wayland screenshot
- `rapidocr` — OCR engine (PaddleOCR v4 via ONNX Runtime)
- `pyzbar` — QR code and barcode detection (via zbar)
- `wl-copy` — Wayland clipboard
- `notify-send` — Desktop notifications (save confirmation, no text detected)
- GTK4 + gtk4-layer-shell — Overlay window
- `slurp` — Region/window selection (for `--region` and `--window` modes)
- `niri` or `swaymsg` — Focused output and window detection (auto-detected)
