"""Tests for screenshot capture."""

from __future__ import annotations

import subprocess
from pathlib import Path
from unittest.mock import patch

import pytest

from live_text.screenshot import capture_full_screen, get_focused_output


def _fake_grim(cmd: list[str], **kwargs: object) -> subprocess.CompletedProcess[str]:
    """Simulate grim by touching the output file."""
    Path(cmd[-1]).touch()
    return subprocess.CompletedProcess(cmd, 0)


class TestGetFocusedOutput:
    def test_niri_success(self) -> None:
        with patch("live_text.screenshot.subprocess.run") as mock_run:
            mock_run.return_value = subprocess.CompletedProcess(
                [], 0, stdout='{"name": "eDP-1", "make": "BOE"}\n'
            )
            assert get_focused_output() == "eDP-1"

    def test_niri_not_found_falls_back_to_sway(self) -> None:
        call_count = 0

        def fake_run(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            nonlocal call_count
            call_count += 1
            if cmd[0] == "niri":
                raise FileNotFoundError("niri")
            return subprocess.CompletedProcess(
                cmd, 0, stdout='[{"name": "HDMI-A-1", "focused": true}]'
            )

        with patch("live_text.screenshot.subprocess.run", side_effect=fake_run):
            assert get_focused_output() == "HDMI-A-1"
            assert call_count == 2

    def test_both_fail_returns_none(self) -> None:
        with patch(
            "live_text.screenshot.subprocess.run",
            side_effect=FileNotFoundError("not found"),
        ):
            assert get_focused_output() is None

    def test_niri_empty_name_falls_back(self) -> None:
        """Empty output name should not be passed to grim -o."""
        call_count = 0

        def fake_run(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            nonlocal call_count
            call_count += 1
            if cmd[0] == "niri":
                return subprocess.CompletedProcess(cmd, 0, stdout='{"name": ""}\n')
            raise FileNotFoundError("swaymsg")

        with patch("live_text.screenshot.subprocess.run", side_effect=fake_run):
            assert get_focused_output() is None

    def test_niri_invalid_json_falls_back(self) -> None:
        call_count = 0

        def fake_run(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            nonlocal call_count
            call_count += 1
            if cmd[0] == "niri":
                return subprocess.CompletedProcess(cmd, 0, stdout="not json")
            raise FileNotFoundError("swaymsg")

        with patch("live_text.screenshot.subprocess.run", side_effect=fake_run):
            assert get_focused_output() is None


class TestCaptureFullScreen:
    def test_returns_temp_png(self) -> None:
        with patch("live_text.screenshot.subprocess.run", side_effect=_fake_grim):
            result = capture_full_screen()
            try:
                assert result.exists()
                assert result.suffix == ".png"
            finally:
                result.unlink(missing_ok=True)

    def test_grim_failure_cleans_up_temp_file(self) -> None:
        """Regression: temp file must not leak when grim fails."""
        created_paths: list[Path] = []

        def track_and_fail(
            cmd: list[str], **kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            created_paths.append(Path(cmd[-1]))
            raise subprocess.CalledProcessError(1, "grim")

        with patch("live_text.screenshot.subprocess.run", side_effect=track_and_fail):
            with pytest.raises(subprocess.CalledProcessError):
                capture_full_screen()

        for p in created_paths:
            assert not p.exists(), f"Temp file leaked: {p}"

    def test_output_flag(self) -> None:
        """With output= set, grim receives -o; without it, no -o."""
        with patch(
            "live_text.screenshot.subprocess.run", side_effect=_fake_grim
        ) as mock_run:
            result = capture_full_screen(output="eDP-1")
            try:
                cmd = mock_run.call_args[0][0]
                idx = cmd.index("-o")
                assert cmd[idx + 1] == "eDP-1"
            finally:
                result.unlink(missing_ok=True)

        with patch(
            "live_text.screenshot.subprocess.run", side_effect=_fake_grim
        ) as mock_run:
            result = capture_full_screen()
            try:
                assert "-o" not in mock_run.call_args[0][0]
            finally:
                result.unlink(missing_ok=True)
