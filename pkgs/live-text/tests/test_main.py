"""Tests for the main entry point."""

from __future__ import annotations

from pathlib import Path
from unittest.mock import patch

import pytest

from live_text.main import main

TEST_DIR = Path(__file__).parent
TEST_IMAGE = TEST_DIR / "test_image.png"


class TestMainErrorHandling:
    def test_ocr_failure_exits_cleanly(
        self, capsys: pytest.CaptureFixture[str]
    ) -> None:
        """main() should catch OCR exceptions and exit 1."""
        with (
            patch("sys.argv", ["live-text", str(TEST_IMAGE)]),
            patch("live_text.main.run_ocr") as mock_ocr,
        ):
            mock_ocr.side_effect = RuntimeError("model load failed")
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 1
            captured = capsys.readouterr()
            assert "ocr" in captured.err.lower()

    def test_no_text_detected_exits_zero(
        self, capsys: pytest.CaptureFixture[str]
    ) -> None:
        """When OCR finds no text, exit 0 with a message."""
        with (
            patch("sys.argv", ["live-text", str(TEST_IMAGE)]),
            patch("live_text.main.run_ocr", return_value=[]),
            patch("live_text.main._notify") as mock_notify,
        ):
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 0
            assert "no text" in capsys.readouterr().err.lower()
            mock_notify.assert_called_once()


class TestScreenshotCleanup:
    def test_temp_screenshot_cleaned_up(self, tmp_path: Path) -> None:
        """The temp screenshot should be deleted after the overlay exits."""
        fake_screenshot = tmp_path / "live-text-fake.png"
        fake_screenshot.write_bytes(b"fake")

        with (
            patch("sys.argv", ["live-text"]),
            patch("live_text.main.get_focused_output", return_value=None),
            patch("live_text.main.capture_full_screen", return_value=fake_screenshot),
            patch("live_text.main.run_ocr", return_value=[]),
            patch("live_text.main._notify"),
        ):
            with pytest.raises(SystemExit):
                main()

        assert not fake_screenshot.exists(), "Temp screenshot was not cleaned up"
