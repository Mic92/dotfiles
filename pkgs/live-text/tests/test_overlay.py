"""Tests for overlay selection and copy logic.

These tests exercise the actual LiveTextOverlay state machine — selection,
clipboard assembly, and code/word interaction — by manipulating state directly
rather than duplicating the logic in a test helper.
"""

from __future__ import annotations

import subprocess
from pathlib import Path
from unittest.mock import MagicMock, patch

from live_text.barcode import CodeBox
from live_text.ocr import LineBox, WordBox
from live_text.overlay import LiveTextOverlay


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


def _make_codes() -> list[CodeBox]:
    return [
        CodeBox("https://example.com", "QRCODE", 300, 50, 100, 100),
        CodeBox("1234567890", "EAN13", 300, 200, 150, 60),
    ]


def _make_overlay(
    lines: list[LineBox] | None = None,
    codes: list[CodeBox] | None = None,
) -> LiveTextOverlay:
    """Create an overlay with test data, without starting the GTK app."""
    return LiveTextOverlay(
        screenshot_path=Path("/dev/null"),
        lines=lines or [],
        wl_copy_cmd="wl-copy",
        codes=codes,
    )


class TestCopySelectedText:
    """Test _copy_selected_text sends the right data to wl-copy."""

    def _copy_and_capture(self, overlay: LiveTextOverlay) -> str:
        """Run _copy_selected_text and return what was sent to wl-copy."""
        captured: bytes = b""

        def fake_run(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[bytes]:
            nonlocal captured
            captured = kwargs.get("input", b"")  # type: ignore[assignment]
            return subprocess.CompletedProcess(cmd, 0)

        with patch("live_text.overlay.subprocess.run", side_effect=fake_run):
            # Suppress GLib.timeout_add (no GTK main loop in tests)
            with patch("live_text.overlay.GLib.timeout_add"):
                overlay._drawing_area = MagicMock()
                overlay._copy_selected_text()
        return captured.decode()

    def test_copies_single_word(self) -> None:
        overlay = _make_overlay(lines=_make_lines())
        overlay.selected_words = {(0, 0)}
        assert self._copy_and_capture(overlay) == "Hello"

    def test_copies_words_across_lines(self) -> None:
        overlay = _make_overlay(lines=_make_lines())
        overlay.selected_words = {(0, 1), (2, 0)}
        assert self._copy_and_capture(overlay) == "World\nLive"

    def test_copies_full_text(self) -> None:
        lines = _make_lines()
        overlay = _make_overlay(lines=lines)
        overlay.selected_words = {
            (li, wi) for li, line in enumerate(lines) for wi in range(len(line.words))
        }
        assert (
            self._copy_and_capture(overlay) == "Hello World\nThis is a test\nLive Text"
        )

    def test_copies_single_code(self) -> None:
        overlay = _make_overlay(codes=_make_codes())
        overlay.selected_codes = {0}
        assert self._copy_and_capture(overlay) == "https://example.com"

    def test_copies_multiple_codes(self) -> None:
        overlay = _make_overlay(codes=_make_codes())
        overlay.selected_codes = {0, 1}
        assert self._copy_and_capture(overlay) == "https://example.com\n1234567890"

    def test_copies_words_and_codes_together(self) -> None:
        overlay = _make_overlay(lines=_make_lines(), codes=_make_codes())
        overlay.selected_words = {(0, 0)}
        overlay.selected_codes = {1}
        text = self._copy_and_capture(overlay)
        assert text == "Hello\n1234567890"

    def test_nothing_copied_when_no_selection(self) -> None:
        overlay = _make_overlay(lines=_make_lines(), codes=_make_codes())
        with patch("live_text.overlay.subprocess.run") as mock_run:
            overlay._drawing_area = MagicMock()
            overlay._copy_selected_text()
            mock_run.assert_not_called()


class TestHitDetection:
    """Test that _hit_word and _hit_code find the right targets."""

    def test_hit_word(self) -> None:
        overlay = _make_overlay(lines=_make_lines())
        # Click inside "World" (x=70..125, y=50..65)
        assert overlay._hit_word(80, 55) == (0, 1)
        # Click inside "test" (x=107..147, y=100..115)
        assert overlay._hit_word(120, 105) == (1, 3)
        # Click on empty space
        assert overlay._hit_word(200, 200) is None

    def test_hit_code(self) -> None:
        overlay = _make_overlay(codes=_make_codes())
        # Click inside first code (x=300..400, y=50..150)
        assert overlay._hit_code(350, 100) == 0
        # Click inside second code (x=300..450, y=200..260)
        assert overlay._hit_code(400, 230) == 1
        # Click on empty space
        assert overlay._hit_code(10, 10) is None

    def test_word_takes_priority_when_overlapping_code(self) -> None:
        """When a word and code overlap, clicking should find both independently."""
        lines = [LineBox(words=(_w("overlap", 300, 50, 80, 15),))]
        codes = [CodeBox("data", "QRCODE", 300, 50, 100, 100)]
        overlay = _make_overlay(lines=lines, codes=codes)
        # Both should be detectable at same coords
        assert overlay._hit_word(340, 55) == (0, 0)
        assert overlay._hit_code(340, 55) == 0


class TestHasSelection:
    def test_no_selection(self) -> None:
        overlay = _make_overlay(lines=_make_lines(), codes=_make_codes())
        assert not overlay._has_selection()

    def test_word_selected(self) -> None:
        overlay = _make_overlay(lines=_make_lines())
        overlay.selected_words = {(0, 0)}
        assert overlay._has_selection()

    def test_code_selected(self) -> None:
        overlay = _make_overlay(codes=_make_codes())
        overlay.selected_codes = {0}
        assert overlay._has_selection()


class TestSetCodes:
    """Test that set_codes invalidates stale selections."""

    def test_clears_selected_codes_on_update(self) -> None:
        overlay = _make_overlay(codes=_make_codes())
        overlay.selected_codes = {0, 1}

        new_codes = [CodeBox("new", "QRCODE", 0, 0, 50, 50)]
        overlay._drawing_area = MagicMock()
        overlay._apply_codes(new_codes)

        assert overlay.codes == new_codes
        assert overlay.selected_codes == set()


class TestSetLines:
    """Test that set_lines invalidates stale selections."""

    def test_clears_selected_words_on_update(self) -> None:
        overlay = _make_overlay(lines=_make_lines())
        overlay.selected_words = {(0, 0), (1, 3)}

        new_lines = [LineBox(words=(_w("New", 0, 0, 30, 10),))]
        overlay._drawing_area = MagicMock()
        overlay._apply_lines(new_lines)

        assert overlay.lines == new_lines
        assert overlay.selected_words == set()
