"""OCR processing using RapidOCR (PaddleOCR models via ONNX Runtime)."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class WordBox:
    """A detected word with its bounding box coordinates."""

    text: str
    x: int
    y: int
    width: int
    height: int
    confidence: float

    @property
    def x2(self) -> int:
        return self.x + self.width

    @property
    def y2(self) -> int:
        return self.y + self.height

    def contains_point(self, px: float, py: float) -> bool:
        return self.x <= px <= self.x2 and self.y <= py <= self.y2

    def intersects_rect(self, rx: int, ry: int, rw: int, rh: int) -> bool:
        return not (
            self.x2 < rx or self.x > rx + rw or self.y2 < ry or self.y > ry + rh
        )


@dataclass(frozen=True)
class LineBox:
    """A line of text composed of word boxes."""

    words: tuple[WordBox, ...]

    @property
    def text(self) -> str:
        return " ".join(w.text for w in self.words)

    @property
    def x(self) -> int:
        return min(w.x for w in self.words)

    @property
    def y(self) -> int:
        return min(w.y for w in self.words)

    @property
    def x2(self) -> int:
        return max(w.x2 for w in self.words)

    @property
    def y2(self) -> int:
        return max(w.y2 for w in self.words)

    @property
    def width(self) -> int:
        return self.x2 - self.x

    @property
    def height(self) -> int:
        return self.y2 - self.y

    def contains_point(self, px: float, py: float) -> bool:
        return self.x <= px <= self.x2 and self.y <= py <= self.y2

    def intersects_rect(self, rx: int, ry: int, rw: int, rh: int) -> bool:
        """Check if this line box intersects with a rectangle."""
        return not (
            self.x2 < rx or self.x > rx + rw or self.y2 < ry or self.y > ry + rh
        )


def _split_line_into_words(
    text: str, box: list[list[float]], confidence: float
) -> list[WordBox]:
    """Split a line's text into word boxes by dividing the bounding box.

    RapidOCR gives us one bounding box per line.  We split the text on
    whitespace and proportionally assign horizontal spans to each word
    based on character count.  This gives reasonable word-level boxes for
    click/drag selection.
    """
    words = text.split()
    if not words:
        return []

    # Convert 4-point polygon to axis-aligned rect
    xs = [p[0] for p in box]
    ys = [p[1] for p in box]
    lx = int(min(xs))
    ly = int(min(ys))
    lw = int(max(xs)) - lx
    lh = int(max(ys)) - ly

    if len(words) == 1:
        return [WordBox(words[0], lx, ly, lw, lh, confidence)]

    # Proportional split: each word gets width proportional to its char count.
    # Add 1 char per inter-word gap to account for spaces in the original.
    total_chars = sum(len(w) for w in words) + len(words) - 1
    if total_chars == 0:
        return []

    result: list[WordBox] = []
    cx = float(lx)  # current x position
    for i, word in enumerate(words):
        # Characters this word "occupies" including the trailing space
        # (except for the last word)
        chars = len(word) + (1 if i < len(words) - 1 else 0)
        w = lw * chars / total_chars
        result.append(WordBox(word, int(cx), ly, max(1, int(w)), lh, confidence))
        cx += w

    return result


def run_ocr(image_path: Path) -> list[LineBox]:
    """Run RapidOCR on an image and return lines with word-level boxes.

    Uses the English recognition model (set via LIVE_TEXT_REC_MODEL env var)
    which preserves spaces in Latin text.  Falls back to the bundled Chinese
    model if the env var is not set.
    """
    import os

    from rapidocr import RapidOCR  # lazy import to avoid slow startup cost

    params: dict[str, str] = {}
    rec_model = os.environ.get("LIVE_TEXT_REC_MODEL")
    if rec_model:
        params["Rec.model_path"] = rec_model

    engine = RapidOCR(params=params if params else None)
    result = engine(str(image_path))

    if not result.txts:
        return []

    lines: list[LineBox] = []
    for text, score, box in zip(result.txts, result.scores, result.boxes):
        text = text.strip()
        if not text:
            continue
        words = _split_line_into_words(text, box.tolist(), score)
        if words:
            lines.append(LineBox(words=tuple(words)))

    lines.sort(key=lambda ln: (ln.y, ln.x))
    return lines
