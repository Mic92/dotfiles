#!/usr/bin/env python3
"""Integration tests for buildbot_pr_check using VCR.py for HTTP request recording."""

import sys
from pathlib import Path

import vcr

# Add parent directory to path to import the module
sys.path.insert(0, str(Path(__file__).parent.parent))

import buildbot_pr_check


# Configure VCR
vcr_config = vcr.VCR(
    cassette_library_dir="tests/cassettes",
    record_mode="once",  # Change to "new_episodes" to re-record
    match_on=["uri", "method"],
    filter_headers=["authorization"],  # Don't record auth tokens
)


class TestIntegration:
    """Integration tests for the two pull requests."""

    @vcr_config.use_cassette("github_pr_459.yaml")
    def test_github_pr_459(self, capsys):
        """Test GitHub PR #459 which has canceled builds."""
        exit_code = buildbot_pr_check.check_pr(
            "https://github.com/TUM-DSE/doctor-cluster-config/pull/459"
        )

        # Should return 1 because there are canceled builds
        assert exit_code == 1

        captured = capsys.readouterr()
        output = captured.out

        # Verify key output elements
        assert "Checking PR #459 in TUM-DSE/doctor-cluster-config (github)" in output
        assert "Found 20 buildbot build(s)" in output
        assert "CANCELLED: 20 builds" in output
        assert "❌ Issues found:" in output
        assert "20 builds were canceled" in output

    @vcr_config.use_cassette("gitea_pr_4210.yaml")
    def test_gitea_pr_4210(self, capsys):
        """Test Gitea PR #4210 which has all successful builds."""
        exit_code = buildbot_pr_check.check_pr(
            "https://git.clan.lol/clan/clan-core/pulls/4210"
        )

        # Should return 0 because all builds passed
        assert exit_code == 0

        captured = capsys.readouterr()
        output = captured.out

        # Verify key output elements
        assert "Checking PR #4210 in clan/clan-core (gitea)" in output
        assert "Found 1 buildbot build(s)" in output
        assert "SUCCESS: 34 builds" in output
        assert "✅ All 34 builds passed successfully!" in output
