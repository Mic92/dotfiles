"""Tests for overlay text assembly logic."""

from __future__ import annotations

from live_text.ocr import LineBox, WordBox


def _w(
    text: str, x: int, y: int, w: int, h: int, *, block: int = 1, word_num: int = 1
) -> WordBox:
    return WordBox(text, x, y, w, h, 90.0, block, 1, 1, word_num)


def _make_lines() -> list[LineBox]:
    return [
        LineBox(
            words=(
                _w("Hello", 10, 50, 50, 15, word_num=1),
                _w("World", 70, 50, 55, 15, word_num=2),
            ),
            block_num=1,
            par_num=1,
            line_num=1,
        ),
        LineBox(
            words=(
                _w("This", 10, 100, 40, 15, block=2, word_num=1),
                _w("is", 60, 100, 15, 15, block=2, word_num=2),
                _w("a", 85, 105, 12, 10, block=2, word_num=3),
                _w("test", 107, 100, 40, 15, block=2, word_num=4),
            ),
            block_num=2,
            par_num=1,
            line_num=1,
        ),
        LineBox(
            words=(
                _w("Live", 10, 175, 46, 18, block=3, word_num=1),
                _w("Text", 65, 175, 48, 18, block=3, word_num=2),
            ),
            block_num=3,
            par_num=1,
            line_num=1,
        ),
    ]


class TestSelectedTextAssembly:
    """Test the text assembly logic mirroring _copy_and_quit."""

    def test_selected_lines_sorted_by_position(self) -> None:
        lines = _make_lines()
        # Select out of order — should be sorted top-to-bottom
        selected = {2, 0}
        sorted_indices = sorted(selected, key=lambda i: (lines[i].y, lines[i].x))
        text = "\n".join(lines[i].text for i in sorted_indices)
        assert text == "Hello World\nLive Text"

    def test_all_lines_joined(self) -> None:
        lines = _make_lines()
        selected = {0, 1, 2}
        sorted_indices = sorted(selected, key=lambda i: (lines[i].y, lines[i].x))
        text = "\n".join(lines[i].text for i in sorted_indices)
        assert text == "Hello World\nThis is a test\nLive Text"
