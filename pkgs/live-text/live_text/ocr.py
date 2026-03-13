"""OCR processing using Tesseract with word-level bounding boxes."""

from __future__ import annotations

import csv
import io
import subprocess
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
    block_num: int
    par_num: int
    line_num: int
    word_num: int

    @property
    def x2(self) -> int:
        return self.x + self.width

    @property
    def y2(self) -> int:
        return self.y + self.height


@dataclass(frozen=True)
class LineBox:
    """A line of text composed of word boxes."""

    words: tuple[WordBox, ...]
    block_num: int
    par_num: int
    line_num: int

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


def run_ocr(
    image_path: Path,
    tesseract_cmd: str = "tesseract",
    lang: str = "eng",
) -> list[WordBox]:
    """Run Tesseract OCR on an image and return word-level bounding boxes.

    Uses TSV output mode for structured data with coordinates.
    """
    result = subprocess.run(
        [tesseract_cmd, str(image_path), "stdout", "-l", lang, "--psm", "3", "tsv"],
        capture_output=True,
        text=True,
        check=True,
    )

    words: list[WordBox] = []
    reader = csv.DictReader(io.StringIO(result.stdout), delimiter="\t")

    for row in reader:
        # level 5 = word level in Tesseract TSV output
        if row.get("level") != "5":
            continue

        text = row.get("text", "").strip()
        if not text:
            continue

        conf = float(row.get("conf", "0"))
        if conf < 10:
            continue

        words.append(
            WordBox(
                text=text,
                x=int(row["left"]),
                y=int(row["top"]),
                width=int(row["width"]),
                height=int(row["height"]),
                confidence=conf,
                block_num=int(row["block_num"]),
                par_num=int(row["par_num"]),
                line_num=int(row["line_num"]),
                word_num=int(row["word_num"]),
            )
        )

    return words


def group_into_lines(words: list[WordBox]) -> list[LineBox]:
    """Group words into lines based on Tesseract's block/par/line structure."""
    lines_dict: dict[tuple[int, int, int], list[WordBox]] = {}

    for word in words:
        key = (word.block_num, word.par_num, word.line_num)
        lines_dict.setdefault(key, []).append(word)

    lines: list[LineBox] = []
    for (block_num, par_num, line_num), line_words in sorted(lines_dict.items()):
        sorted_words = sorted(line_words, key=lambda w: w.x)
        lines.append(
            LineBox(
                words=tuple(sorted_words),
                block_num=block_num,
                par_num=par_num,
                line_num=line_num,
            )
        )

    return lines
