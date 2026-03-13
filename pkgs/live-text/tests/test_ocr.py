"""Tests for OCR processing."""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path
from unittest.mock import patch

import pytest

from live_text.ocr import LineBox, WordBox, group_into_lines, run_ocr

TEST_DIR = Path(__file__).parent
TEST_IMAGE = TEST_DIR / "test_image.png"


def has_tesseract() -> bool:
    return shutil.which("tesseract") is not None


def _word(
    text: str,
    x: int,
    y: int,
    width: int,
    height: int,
    *,
    confidence: float = 90.0,
    block_num: int = 1,
    par_num: int = 1,
    line_num: int = 1,
    word_num: int = 1,
) -> WordBox:
    """Create a WordBox with sensible defaults for tests."""
    return WordBox(
        text=text,
        x=x,
        y=y,
        width=width,
        height=height,
        confidence=confidence,
        block_num=block_num,
        par_num=par_num,
        line_num=line_num,
        word_num=word_num,
    )


def _make_line() -> LineBox:
    words = (
        _word("Hello", 10, 50, 50, 15, word_num=1),
        _word("World", 70, 50, 55, 15, word_num=2),
    )
    return LineBox(words=words, block_num=1, par_num=1, line_num=1)


class TestLineBox:
    def test_contains_point(self) -> None:
        line = _make_line()
        # Inside
        assert line.contains_point(50, 55)
        # Edges (inclusive — important for click hit-testing)
        assert line.contains_point(10, 50)
        assert line.contains_point(125, 65)
        # Outside
        assert not line.contains_point(5, 55)
        assert not line.contains_point(50, 70)

    def test_intersects_rect(self) -> None:
        line = _make_line()
        assert line.intersects_rect(0, 40, 200, 30)  # overlap
        assert line.intersects_rect(0, 0, 200, 200)  # containing
        assert not line.intersects_rect(200, 200, 50, 50)  # no overlap


class TestGroupIntoLines:
    def test_groups_and_sorts_words(self) -> None:
        words = [
            # Line 2, word 1 — deliberately first to test sorting
            _word("Second", 10, 100, 60, 15, line_num=2, word_num=1),
            # Line 1, word 2 — out of x-order to test word sorting
            _word("World", 70, 50, 55, 15, line_num=1, word_num=2),
            _word("Hello", 10, 50, 50, 15, line_num=1, word_num=1),
            _word("line", 80, 100, 40, 15, line_num=2, word_num=2),
        ]
        lines = group_into_lines(words)
        assert len(lines) == 2
        assert lines[0].text == "Hello World"
        assert lines[1].text == "Second line"

    def test_empty_input(self) -> None:
        assert group_into_lines([]) == []


# Header shared by all crafted TSV fixtures
_TSV_HEADER = "level\tpage_num\tblock_num\tpar_num\tline_num\tword_num\tleft\ttop\twidth\theight\tconf\ttext"


class TestRunOCRParsing:
    """Test TSV parsing with controlled input (no real tesseract)."""

    def _run_with_tsv(self, *rows: str) -> list[WordBox]:
        tsv = "\n".join([_TSV_HEADER, *rows, ""])
        with patch("live_text.ocr.subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess([], 0, stdout=tsv)
            return run_ocr(Path("/fake.png"))

    def test_parses_word_level_rows(self) -> None:
        words = self._run_with_tsv(
            "5\t1\t1\t1\t1\t1\t10\t20\t50\t15\t92.5\tHello",
            "5\t1\t1\t1\t1\t2\t70\t20\t55\t15\t88.0\tWorld",
        )
        assert len(words) == 2
        assert words[0].text == "Hello"
        assert words[0].x == 10
        assert words[0].confidence == 92.5
        assert words[1].text == "World"

    def test_skips_low_confidence(self) -> None:
        words = self._run_with_tsv(
            "5\t1\t1\t1\t1\t1\t10\t20\t50\t15\t5.0\tgarbage",
            "5\t1\t1\t1\t1\t2\t70\t20\t55\t15\t90.0\tgood",
        )
        assert len(words) == 1
        assert words[0].text == "good"


@pytest.mark.skipif(not has_tesseract(), reason="tesseract not installed")
class TestRunOCR:
    def test_detects_text_from_image(self) -> None:
        words = run_ocr(TEST_IMAGE)
        texts = [w.text for w in words]

        for expected in ("Hello", "World", "This", "test", "Live", "Text", "Linux"):
            assert expected in texts

        # Sanity-check coordinates
        for word in words:
            assert word.x >= 0 and word.y >= 0
            assert word.width > 0 and word.height > 0
            assert word.confidence >= 10

    def test_group_into_lines_from_real_ocr(self) -> None:
        words = run_ocr(TEST_IMAGE)
        lines = group_into_lines(words)

        assert len(lines) >= 3
        all_text = " ".join(line.text for line in lines)
        for expected in ("Hello", "World", "Live", "Linux"):
            assert expected in all_text

    def test_lang_passed_to_tesseract(self) -> None:
        """Verify that the lang parameter is forwarded to tesseract."""
        with patch("live_text.ocr.subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess(
                [],
                0,
                stdout=_TSV_HEADER + "\n",
            )
            run_ocr(TEST_IMAGE, lang="deu")
            cmd = mock_run.call_args[0][0]
            lang_idx = cmd.index("-l")
            assert cmd[lang_idx + 1] == "deu"
