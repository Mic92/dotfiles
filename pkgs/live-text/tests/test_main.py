"""Tests for the main entry point."""

from __future__ import annotations

import subprocess
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

    def test_region_opens_overlay(self, tmp_path: Path) -> None:
        """Region mode should capture a screenshot and open the overlay UI."""
        import subprocess

        fake_screenshot = tmp_path / "region.png"
        fake_screenshot.write_bytes(b"fake")

        mock_overlay_cls = MagicMock()
        call_count = 0

        def mock_run_side_effect(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            nonlocal call_count
            call_count += 1
            if cmd[0] == "slurp":
                return subprocess.CompletedProcess(cmd, 0, stdout="100,100 200x200\n")
            if cmd[0] == "grim":
                # Write fake data to the output path
                out_path = Path(cmd[-1])
                out_path.write_bytes(b"fake-png")
                return subprocess.CompletedProcess(cmd, 0)
            return subprocess.CompletedProcess(cmd, 0)

        with (
            patch("sys.argv", ["live-text", "--region"]),
            patch("live_text.main.subprocess.run", side_effect=mock_run_side_effect),
            patch("live_text.overlay.LiveTextOverlay", mock_overlay_cls),
            patch("live_text.ocr.run_ocr", return_value=[]),
        ):
            main()

        mock_overlay_cls.assert_called_once()
        _, kwargs = mock_overlay_cls.call_args
        assert kwargs["lines"] == []
        mock_overlay_cls.return_value.run.assert_called_once()


class TestWindowMode:
    def test_window_opens_overlay(self, tmp_path: Path) -> None:
        """Window mode should capture a window screenshot and open the overlay."""
        mock_overlay_cls = MagicMock()

        def mock_run_side_effect(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            if cmd[0] == "slurp":
                return subprocess.CompletedProcess(
                    cmd, 0, stdout="0,0 1920x1080 Firefox\n"
                )
            if cmd[0] == "grim":
                out_path = Path(cmd[-1])
                out_path.write_bytes(b"fake-png")
                return subprocess.CompletedProcess(cmd, 0)
            return subprocess.CompletedProcess(cmd, 0)

        with (
            patch("sys.argv", ["live-text", "--window"]),
            patch("live_text.main.subprocess.run", side_effect=mock_run_side_effect),
            patch(
                "live_text.main.get_window_geometries",
                return_value=["0,0 1920x1080 Firefox", "100,100 800x600 Terminal"],
            ),
            patch("live_text.overlay.LiveTextOverlay", mock_overlay_cls),
            patch("live_text.ocr.run_ocr", return_value=[]),
        ):
            main()

        mock_overlay_cls.assert_called_once()
        _, kwargs = mock_overlay_cls.call_args
        assert kwargs["lines"] == []
        mock_overlay_cls.return_value.run.assert_called_once()

    def test_no_windows_exits(self, capsys: pytest.CaptureFixture[str]) -> None:
        """If no windows are found, exit with error."""
        with (
            patch("sys.argv", ["live-text", "--window"]),
            patch("live_text.main.get_window_geometries", return_value=[]),
        ):
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 1
            assert "window list" in capsys.readouterr().err.lower()

    def test_slurp_cancel_exits_zero(self) -> None:
        """If user cancels slurp window selection, exit 0."""
        with (
            patch("sys.argv", ["live-text", "--window"]),
            patch(
                "live_text.main.get_window_geometries",
                return_value=["0,0 800x600 vim"],
            ),
            patch("live_text.main.subprocess.run") as mock_run,
        ):
            mock_run.side_effect = subprocess.CalledProcessError(1, "slurp")
            with pytest.raises(SystemExit) as exc_info:
                main()
            assert exc_info.value.code == 0

    def test_slurp_receives_window_geometries(self) -> None:
        """Slurp should receive window geometries on stdin."""
        mock_overlay_cls = MagicMock()
        slurp_input_received: str | None = None

        def mock_run_side_effect(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            nonlocal slurp_input_received
            if cmd[0] == "slurp":
                slurp_input_received = str(kwargs.get("input", ""))
                return subprocess.CompletedProcess(cmd, 0, stdout="0,0 800x600 vim\n")
            if cmd[0] == "grim":
                Path(cmd[-1]).write_bytes(b"fake")
                return subprocess.CompletedProcess(cmd, 0)
            return subprocess.CompletedProcess(cmd, 0)

        with (
            patch("sys.argv", ["live-text", "--window"]),
            patch("live_text.main.subprocess.run", side_effect=mock_run_side_effect),
            patch(
                "live_text.main.get_window_geometries",
                return_value=["0,0 800x600 vim", "900,0 800x600 firefox"],
            ),
            patch("live_text.overlay.LiveTextOverlay", mock_overlay_cls),
            patch("live_text.ocr.run_ocr", return_value=[]),
        ):
            main()

        assert slurp_input_received is not None
        assert "0,0 800x600 vim" in slurp_input_received
        assert "900,0 800x600 firefox" in slurp_input_received

    def test_label_stripped_from_geometry(self) -> None:
        """The window title label should be stripped before passing to grim."""
        mock_overlay_cls = MagicMock()
        grim_geometry: str | None = None

        def mock_run_side_effect(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            nonlocal grim_geometry
            if cmd[0] == "slurp":
                return subprocess.CompletedProcess(
                    cmd, 0, stdout="100,200 800x600 My Window Title\n"
                )
            if cmd[0] == "grim":
                grim_geometry = cmd[cmd.index("-g") + 1]
                Path(cmd[-1]).write_bytes(b"fake")
                return subprocess.CompletedProcess(cmd, 0)
            return subprocess.CompletedProcess(cmd, 0)

        with (
            patch("sys.argv", ["live-text", "--window"]),
            patch("live_text.main.subprocess.run", side_effect=mock_run_side_effect),
            patch(
                "live_text.main.get_window_geometries",
                return_value=["100,200 800x600 My Window Title"],
            ),
            patch("live_text.overlay.LiveTextOverlay", mock_overlay_cls),
            patch("live_text.ocr.run_ocr", return_value=[]),
        ):
            main()

        assert grim_geometry == "100,200 800x600"


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
