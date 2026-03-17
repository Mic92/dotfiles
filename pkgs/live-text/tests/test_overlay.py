"""Tests for overlay text assembly logic."""

from __future__ import annotations

from live_text.ocr import LineBox, WordBox


def _w(text: str, x: int, y: int, w: int, h: int) -> WordBox:
    return WordBox(text, x, y, w, h, 90.0)


def _make_lines() -> list[LineBox]:
    return [
        LineBox(
            words=(
                _w("Hello", 10, 50, 50, 15),
                _w("World", 70, 50, 55, 15),
            ),
        ),
        LineBox(
            words=(
                _w("This", 10, 100, 40, 15),
                _w("is", 60, 100, 15, 15),
                _w("a", 85, 105, 12, 10),
                _w("test", 107, 100, 40, 15),
            ),
        ),
        LineBox(
            words=(
                _w("Live", 10, 175, 46, 18),
                _w("Text", 65, 175, 48, 18),
            ),
        ),
    ]


def _assemble_text(lines: list[LineBox], selected: set[tuple[int, int]]) -> str:
    """Reproduce the overlay's _copy_selection text assembly."""
    line_texts: dict[int, list[str]] = {}
    for li, wi in sorted(selected):
        line_texts.setdefault(li, []).append(lines[li].words[wi].text)
    return "\n".join(" ".join(words) for words in line_texts.values())


class TestSelectedTextAssembly:
    """Test the word-level text assembly logic mirroring _copy_selection."""

    def test_single_word(self) -> None:
        lines = _make_lines()
        assert _assemble_text(lines, {(0, 0)}) == "Hello"

    def test_partial_line_selection(self) -> None:
        lines = _make_lines()
        # Select "is a" from the second line
        assert _assemble_text(lines, {(1, 1), (1, 2)}) == "is a"

    def test_words_across_lines(self) -> None:
        lines = _make_lines()
        # Select "World" from line 0 and "Live" from line 2
        selected = {(0, 1), (2, 0)}
        assert _assemble_text(lines, selected) == "World\nLive"

    def test_full_selection_sorted(self) -> None:
        lines = _make_lines()
        # Select all words
        selected = {
            (li, wi) for li, line in enumerate(lines) for wi in range(len(line.words))
        }
        text = _assemble_text(lines, selected)
        assert text == "Hello World\nThis is a test\nLive Text"
