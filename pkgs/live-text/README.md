# live-text

Interactive OCR overlay for Wayland (niri, sway, etc.) — the Linux equivalent of
macOS Live Text.

Takes a full-screen screenshot, runs OCR using RapidOCR (PaddleOCR v4 models via
ONNX Runtime), then shows an interactive overlay where you can click or
drag-select text regions. Selected text is copied to clipboard with Ctrl+C.

## Usage

```bash
# Capture screen and show overlay
live-text

# OCR an existing image file
live-text screenshot.png
```

## Keybindings (in overlay)

| Key              | Action                                  |
| ---------------- | --------------------------------------- |
| Click line       | Select it (replaces previous selection) |
| Shift+click      | Add/remove line from selection          |
| Drag             | Select all lines in rectangle           |
| Ctrl+C / Enter   | Copy selection to clipboard             |
| Ctrl+A           | Select all lines                        |
| Click empty area | Clear selection                         |
| Escape           | Quit                                    |

A brief white flash confirms successful copy.

## Dependencies

- `grim` — Wayland screenshot
- `rapidocr` — OCR engine (PaddleOCR v4 via ONNX Runtime)
- `wl-copy` — Wayland clipboard
- `notify-send` — Desktop notifications (only for "no text detected")
- GTK4 + gtk4-layer-shell — Overlay window
