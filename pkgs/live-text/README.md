# live-text

Interactive OCR overlay for Wayland (niri, sway, etc.) — the Linux equivalent of
macOS Live Text.

Takes a screenshot (or reads an image from stdin/file), runs OCR using RapidOCR
(PaddleOCR v4 via ONNX Runtime), then shows an interactive overlay where you can
select text at the word level. Selected text is copied to clipboard with Ctrl+C.

## Usage

```bash
# Capture full screen and show overlay
live-text

# OCR an existing image file
live-text screenshot.png

# Pipeline mode: read from stdin
grim - | live-text -

# Region OCR: select area first, then OCR just that region
slurp | grim -g - - | live-text -
```

## Keybindings (in overlay)

| Key              | Action                                  |
| ---------------- | --------------------------------------- |
| Click word       | Select it (replaces previous selection) |
| Shift+click      | Add/remove word from selection          |
| Drag             | Select all words in swept area          |
| Ctrl+C / Enter   | Copy selection to clipboard             |
| Ctrl+A           | Select all words                        |
| Click empty area | Clear selection                         |
| Escape           | Quit                                    |

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

## Dependencies

- `grim` — Wayland screenshot
- `rapidocr` — OCR engine (PaddleOCR v4 via ONNX Runtime)
- `wl-copy` — Wayland clipboard
- `notify-send` — Desktop notifications (only for "no text detected")
- GTK4 + gtk4-layer-shell — Overlay window
- `slurp` — Region selection (optional, for pipeline mode)
- `satty` — Annotation tool (optional, for screenshot-annotate)
