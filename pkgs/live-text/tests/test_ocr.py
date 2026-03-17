"""Tests for OCR processing."""

from __future__ import annotations

from pathlib import Path

import pytest

from live_text.ocr import LineBox, WordBox, _split_line_into_words, run_ocr

TEST_DIR = Path(__file__).parent
TEST_IMAGE = TEST_DIR / "test_image.png"


def _word(
    text: str,
    x: int,
    y: int,
    width: int,
    height: int,
    *,
    confidence: float = 90.0,
) -> WordBox:
    """Create a WordBox with sensible defaults for tests."""
    return WordBox(
        text=text,
        x=x,
        y=y,
        width=width,
        height=height,
        confidence=confidence,
    )


def _make_line() -> LineBox:
    words = (
        _word("Hello", 10, 50, 50, 15),
        _word("World", 70, 50, 55, 15),
    )
    return LineBox(words=words)


class TestWordBox:
    def test_contains_point(self) -> None:
        w = _word("test", 10, 20, 50, 15)
        assert w.contains_point(30, 25)  # inside
        assert w.contains_point(10, 20)  # top-left edge
        assert w.contains_point(60, 35)  # bottom-right edge
        assert not w.contains_point(5, 25)  # left of
        assert not w.contains_point(30, 40)  # below

    def test_intersects_rect(self) -> None:
        w = _word("test", 10, 20, 50, 15)
        assert w.intersects_rect(0, 0, 100, 100)  # containing
        assert w.intersects_rect(30, 25, 10, 5)  # inside word
        assert w.intersects_rect(55, 30, 20, 20)  # partial overlap
        assert not w.intersects_rect(70, 40, 10, 10)  # no overlap


class TestLineBox:
    def test_contains_point(self) -> None:
        line = _make_line()
        assert line.contains_point(50, 55)
        assert line.contains_point(10, 50)
        assert line.contains_point(125, 65)
        assert not line.contains_point(5, 55)
        assert not line.contains_point(50, 70)

    def test_intersects_rect(self) -> None:
        line = _make_line()
        assert line.intersects_rect(0, 40, 200, 30)
        assert line.intersects_rect(0, 0, 200, 200)
        assert not line.intersects_rect(200, 200, 50, 50)


class TestSplitLineIntoWords:
    """Test proportional word splitting from line bounding box."""

    def test_single_word(self) -> None:
        box = [[10.0, 20.0], [110.0, 20.0], [110.0, 40.0], [10.0, 40.0]]
        words = _split_line_into_words("Hello", box, 0.95)
        assert len(words) == 1
        assert words[0].text == "Hello"
        assert words[0].x == 10
        assert words[0].width == 100

    def test_two_equal_words(self) -> None:
        # "ab cd" → 2+1 + 2 = 5 total chars (including space)
        box = [[0.0, 0.0], [100.0, 0.0], [100.0, 20.0], [0.0, 20.0]]
        words = _split_line_into_words("ab cd", box, 0.9)
        assert len(words) == 2
        assert words[0].text == "ab"
        assert words[1].text == "cd"
        # "ab" gets 3/5 of 100 = 60, "cd" gets 2/5 of 100 = 40
        assert words[0].width == 60
        assert words[1].x == 60
        assert words[1].width == 40

    def test_empty_text(self) -> None:
        box = [[0.0, 0.0], [100.0, 0.0], [100.0, 20.0], [0.0, 20.0]]
        assert _split_line_into_words("", box, 0.9) == []
        assert _split_line_into_words("   ", box, 0.9) == []

    def test_rotated_polygon(self) -> None:
        """4-point polygon → axis-aligned bounding rect."""
        box = [[12.0, 18.0], [62.0, 20.0], [63.0, 37.0], [13.0, 35.0]]
        words = _split_line_into_words("tilted", box, 0.95)
        assert len(words) == 1
        assert words[0].x == 12
        assert words[0].y == 18
        assert words[0].width == 51  # 63 - 12
        assert words[0].height == 19  # 37 - 18

    def test_words_span_full_width(self) -> None:
        """All word boxes together should approximately cover the line width."""
        box = [[10.0, 0.0], [210.0, 0.0], [210.0, 20.0], [10.0, 20.0]]
        words = _split_line_into_words("Hello World Test", box, 0.9)
        assert words[0].x == 10
        last = words[-1]
        # Last word should end near the line's right edge
        assert abs((last.x + last.width) - 210) < 5


def _has_rapidocr() -> bool:
    try:
        import rapidocr  # noqa: F401

        return True
    except ImportError:
        return False


@pytest.mark.skipif(not _has_rapidocr(), reason="rapidocr not installed")
class TestRunOCR:
    def test_detects_text_from_image(self) -> None:
        lines = run_ocr(TEST_IMAGE)

        all_text = " ".join(ln.text for ln in lines)
        for expected in ("Hello", "World", "This", "test", "Live", "Text", "Linux"):
            assert expected in all_text, f"{expected!r} not found in {all_text!r}"

    def test_word_level_boxes_have_valid_coords(self) -> None:
        lines = run_ocr(TEST_IMAGE)

        for line in lines:
            for word in line.words:
                assert word.x >= 0 and word.y >= 0
                assert word.width > 0 and word.height > 0
                assert word.confidence > 0

    def test_lines_sorted_top_to_bottom(self) -> None:
        lines = run_ocr(TEST_IMAGE)
        assert len(lines) >= 3
        ys = [ln.y for ln in lines]
        assert ys == sorted(ys), f"Lines not sorted top-to-bottom: {ys}"

    def test_words_sorted_left_to_right(self) -> None:
        lines = run_ocr(TEST_IMAGE)
        for line in lines:
            if len(line.words) > 1:
                xs = [w.x for w in line.words]
                assert xs == sorted(xs), f"Words not sorted L-R: {line.text}"

    def test_multi_word_lines_have_word_boxes(self) -> None:
        """Lines with spaces should produce multiple word boxes."""
        lines = run_ocr(TEST_IMAGE)
        multi_word = [ln for ln in lines if " " in ln.text]
        assert len(multi_word) > 0, "Expected at least one multi-word line"
        for line in multi_word:
            assert len(line.words) > 1, f"'{line.text}' should have >1 word box"

    def test_no_text_on_blank_image(self, tmp_path: Path) -> None:
        """A blank white image should produce no lines."""
        from PIL import Image

        blank = Image.new("RGB", (200, 200), "white")
        blank_path = tmp_path / "blank.png"
        blank.save(str(blank_path))

        lines = run_ocr(blank_path)
        assert lines == []
