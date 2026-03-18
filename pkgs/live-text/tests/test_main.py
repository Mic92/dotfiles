"""Tests for the main entry point."""

from __future__ import annotations

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from live_text.main import main

TEST_DIR = Path(__file__).parent
TEST_IMAGE = TEST_DIR / "test_image.png"


class TestMainOCR:
    def test_overlay_starts_with_empty_lines(self) -> None:
        """Overlay should start immediately with empty lines (OCR runs async)."""
        mock_overlay_cls = MagicMock()
        with (
            patch("sys.argv", ["live-text", str(TEST_IMAGE)]),
            patch("live_text.overlay.LiveTextOverlay", mock_overlay_cls),
            patch("live_text.ocr.run_ocr", return_value=[]),
        ):
            main()
        # Overlay is created with empty lines (OCR hasn't finished yet)
        mock_overlay_cls.assert_called_once()
        _, kwargs = mock_overlay_cls.call_args
        assert kwargs["lines"] == []


class TestStdinMode:
    def test_stdin_reads_to_tempfile(self) -> None:
        """Reading from stdin should create a temp file and clean it up."""
        fake_png = b"\x89PNG\r\n\x1a\nfake"
        mock_overlay_cls = MagicMock()

        with (
            patch("sys.argv", ["live-text", "-"]),
            patch("sys.stdin") as mock_stdin,
            patch("live_text.overlay.LiveTextOverlay", mock_overlay_cls),
            patch("live_text.ocr.run_ocr", return_value=[]),
        ):
            mock_stdin.buffer.read.return_value = fake_png
            main()
        mock_overlay_cls.assert_called_once()

    def test_empty_stdin_exits(self, capsys: pytest.CaptureFixture[str]) -> None:
        """Empty stdin should exit with error."""
        with (
            patch("sys.argv", ["live-text", "-"]),
            patch("sys.stdin") as mock_stdin,
        ):
            mock_stdin.buffer.read.return_value = b""
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 1
            assert "no data" in capsys.readouterr().err.lower()


class TestRegionMode:
    def test_slurp_cancel_exits_zero(self) -> None:
        """If user cancels slurp, exit 0."""
        import subprocess

        with (
            patch("sys.argv", ["live-text", "--region"]),
            patch("live_text.main.subprocess.run") as mock_run,
        ):
            mock_run.side_effect = subprocess.CalledProcessError(1, "slurp")
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 0


class TestScreenshotCleanup:
    def test_temp_screenshot_cleaned_up(self, tmp_path: Path) -> None:
        """The temp screenshot should be deleted after the overlay exits."""
        fake_screenshot = tmp_path / "live-text-fake.png"
        fake_screenshot.write_bytes(b"fake")
        mock_overlay_cls = MagicMock()

        with (
            patch("sys.argv", ["live-text"]),
            patch("live_text.main.get_focused_output", return_value=None),
            patch("live_text.main.capture_full_screen", return_value=fake_screenshot),
            patch("live_text.overlay.LiveTextOverlay", mock_overlay_cls),
            patch("live_text.ocr.run_ocr", return_value=[]),
        ):
            main()

        assert not fake_screenshot.exists(), "Temp screenshot was not cleaned up"
