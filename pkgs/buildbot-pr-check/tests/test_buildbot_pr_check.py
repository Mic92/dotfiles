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
        assert "buildbot build(s)" in output

        # Check that we're still finding builds with triggered sub-builds
        assert "build(s) with triggered sub-builds" in output

        # Check for failed builds with flake attributes
        if "Failed builds" in output:
            # Should display flake attributes like "checks.x86_64-linux.nixos-martha"
            assert "checks." in output or "nixos-" in output
            # Should NOT display "Request " anymore
            assert "- Request " not in output

            # Check for log URLs
            if "Log URLs:" in output:
                assert "https://buildbot.dse.in.tum.de/api/v2/logs/" in output

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
        assert "SUCCESS:" in output

        # Check that we're finding builds with triggered sub-builds
        assert "build(s) with triggered sub-builds" in output

        # Should NOT show parent build failure for successful builds
        assert "Parent build failed" not in output
        assert "Parent build logs:" not in output

    @vcr_config.use_cassette("github_pr_3016_eval_error.yaml")
    def test_github_pr_3016_eval_error(self, capsys):
        """Test GitHub PR #3016 which has eval errors in parent build."""
        exit_code = buildbot_pr_check.check_pr(
            "https://github.com/Mic92/dotfiles/pull/3016"
        )

        # Should return 1 because parent build failed
        assert exit_code == 1

        captured = capsys.readouterr()
        output = captured.out

        # Verify key output elements
        assert "Checking PR #3016 in Mic92/dotfiles (github)" in output
        assert "Found 1 buildbot build(s)" in output

        # Check that parent build failure is shown
        assert "Parent build failed: FAILURE" in output
        assert "Parent build logs:" in output
        assert "Evaluate flake" in output

        # Should have log URLs from the failed step
        assert "https://buildbot.thalheim.io/api/v2/logs/" in output

        # Verify the log entry format
        assert "• Evaluate flake (stdio):" in output

        # Check that the log URL is properly formatted
        import re

        log_url_pattern = r"https://buildbot\.thalheim\.io/api/v2/logs/\d+/raw_inline"
        assert re.search(log_url_pattern, output), (
            "Should have properly formatted log URLs for eval error"
        )
