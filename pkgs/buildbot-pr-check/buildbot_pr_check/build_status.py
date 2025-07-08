#!/usr/bin/env python3
from enum import IntEnum


class BuildStatus(IntEnum):
    """Build status codes used by Buildbot"""

    SUCCESS = 0
    WARNINGS = 1
    FAILURE = 2
    SKIPPED = 3
    EXCEPTION = 4
    RETRY = 5
    CANCELLED = 6

    @property
    def display_name(self) -> str:
        """Get display name for the status"""
        return self.name

    @property
    def icon(self) -> str:
        """Get icon for the status"""
        icons = {
            BuildStatus.SUCCESS: "✅",
            BuildStatus.WARNINGS: "⚠️",
            BuildStatus.FAILURE: "❌",
            BuildStatus.SKIPPED: "⏭️",
            BuildStatus.EXCEPTION: "💥",
            BuildStatus.RETRY: "🔄",
            BuildStatus.CANCELLED: "⚠️",
        }
        return icons.get(self, "•")

    @property
    def color(self) -> str:
        """Get color for the status"""
        # Import locally to avoid circular dependency
        from .colors import Colors

        colors = {
            BuildStatus.SUCCESS: Colors.GREEN,
            BuildStatus.WARNINGS: Colors.YELLOW,
            BuildStatus.FAILURE: Colors.RED,
            BuildStatus.SKIPPED: Colors.CYAN,
            BuildStatus.EXCEPTION: Colors.RED,
            BuildStatus.RETRY: Colors.YELLOW,
            BuildStatus.CANCELLED: Colors.YELLOW,
        }
        return colors.get(self, Colors.RESET)

    @property
    def title(self) -> str:
        """Get title for the status"""
        titles = {
            BuildStatus.SUCCESS: "Successful builds",
            BuildStatus.WARNINGS: "Builds with warnings",
            BuildStatus.FAILURE: "Failed builds",
            BuildStatus.SKIPPED: "Skipped builds",
            BuildStatus.EXCEPTION: "Builds with exceptions",
            BuildStatus.RETRY: "Retried builds",
            BuildStatus.CANCELLED: "Canceled builds",
        }
        return titles.get(self, f"{self.display_name} builds")


def get_build_status(result_code: int | None) -> BuildStatus | None:
    """Convert Buildbot result code to BuildStatus enum."""
    if result_code is None:
        return None
    try:
        return BuildStatus(result_code)
    except ValueError:
        # Unknown status code
        return None
