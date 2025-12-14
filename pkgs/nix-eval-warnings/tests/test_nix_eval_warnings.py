"""Integration tests for nix_eval_warnings module."""

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

from nix_eval_warnings import (
    EvalWarning,
    get_flake_input_paths,
    parse_nix_eval_output,
    run_nix_eval_jobs,
)

SUBFLAKE = """\
{
  outputs = { self }: {
    lib.broken = builtins.warn "warning from subflake" { value = 1; };
  };
}
"""

MAIN_FLAKE = """\
{{
  inputs.myinput.url = "path:{subflake_path}";

  outputs = {{ self, myinput }}: {{
    packages.x86_64-linux = {{
      with-warning = builtins.warn "test warning message" (derivation {{
        name = "test-pkg";
        system = "x86_64-linux";
        builder = "/bin/sh";
      }});

      no-warning = derivation {{
        name = "clean-pkg";
        system = "x86_64-linux";
        builder = "/bin/sh";
      }};

      uses-input =
        builtins.warn "using input: ${{toString myinput.lib.broken.value}}" (derivation {{
          name = "uses-input";
          system = "x86_64-linux";
          builder = "/bin/sh";
        }});
    }};
  }};
}}
"""


def setup_nix_env(nix_root: Path) -> dict[str, str]:
    """Set up environment variables for nix to work in sandbox."""
    (nix_root / "store").mkdir(parents=True, exist_ok=True)
    (nix_root / "var/log/nix/drvs").mkdir(parents=True, exist_ok=True)
    (nix_root / "var/nix/profiles").mkdir(parents=True, exist_ok=True)
    (nix_root / "state").mkdir(parents=True, exist_ok=True)
    (nix_root / "cache").mkdir(parents=True, exist_ok=True)

    return {
        **os.environ,
        "NIX_STORE_DIR": str(nix_root / "store"),
        "NIX_DATA_DIR": str(nix_root / "share"),
        "NIX_LOG_DIR": str(nix_root / "var/log/nix"),
        "NIX_STATE_DIR": str(nix_root / "state"),
        "NIX_CONF_DIR": str(nix_root / "etc"),
        "XDG_CACHE_HOME": str(nix_root / "cache"),
        "NIX_CONFIG": "substituters =\nconnect-timeout = 0\nsandbox = false\nexperimental-features = nix-command flakes",
        "_NIX_TEST_NO_SANDBOX": "1",
        "NIX_REMOTE": "",
    }


def init_git_repo(path: Path, env: dict[str, str]) -> None:
    """Initialize a git repo and add all files."""
    subprocess.run(["git", "init"], cwd=path, capture_output=True, check=True, env=env)
    subprocess.run(
        ["git", "add", "."], cwd=path, capture_output=True, check=True, env=env
    )


class TestNixEvalWarningsIntegration(unittest.TestCase):
    """Integration tests using real nix-eval-jobs via our module."""

    tmpdir: tempfile.TemporaryDirectory[str]
    flake_dir: Path
    subflake_dir: Path
    nix_root: Path
    env: dict[str, str]
    input_mapping: dict[str, str]
    warnings: list[EvalWarning]

    @classmethod
    def setUpClass(cls) -> None:
        cls.tmpdir = tempfile.TemporaryDirectory()
        base_dir = Path(cls.tmpdir.name)
        cls.flake_dir = base_dir / "flake"
        cls.subflake_dir = base_dir / "subflake"
        cls.nix_root = base_dir / "nix"
        cls.flake_dir.mkdir()
        cls.subflake_dir.mkdir()
        cls.nix_root.mkdir()

        cls.env = setup_nix_env(cls.nix_root)

        # Set environment for our module calls
        os.environ.update(cls.env)

        # Create subflake
        (cls.subflake_dir / "flake.nix").write_text(SUBFLAKE)
        init_git_repo(cls.subflake_dir, cls.env)

        # Create main flake with input
        flake_content = MAIN_FLAKE.format(subflake_path=cls.subflake_dir)
        (cls.flake_dir / "flake.nix").write_text(flake_content)
        init_git_repo(cls.flake_dir, cls.env)

        # Update flake lock
        subprocess.run(
            [
                "nix",
                "--extra-experimental-features",
                "nix-command flakes",
                "flake",
                "update",
            ],
            cwd=cls.flake_dir,
            capture_output=True,
            check=True,
            env=cls.env,
        )
        subprocess.run(
            ["git", "add", "flake.lock"],
            cwd=cls.flake_dir,
            capture_output=True,
            check=True,
            env=cls.env,
        )

        # Use our module functions
        cls.input_mapping = get_flake_input_paths(str(cls.flake_dir))
        cls.warnings, _ = run_nix_eval_jobs(
            f"{cls.flake_dir}#packages", cls.input_mapping
        )

    @classmethod
    def tearDownClass(cls) -> None:
        cls.tmpdir.cleanup()

    def test_warning_count(self) -> None:
        """Test that warnings are detected (with-warning and uses-input)."""
        assert len(self.warnings) == 2, (
            f"Expected 2 warnings, got {len(self.warnings)}: {self.warnings}"
        )

    def test_warning_type_extracted(self) -> None:
        """Test that the warning message is correctly extracted."""
        warning_types = [w.warning_type for w in self.warnings]
        assert "test warning message" in warning_types, (
            f"Expected 'test warning message' in warnings, got: {warning_types}"
        )

    def test_source_location_has_file_and_line(self) -> None:
        """Test that source file and line number are extracted."""
        for w in self.warnings:
            assert "flake.nix:" in w.source, (
                f"Expected 'flake.nix:' in source, got: {w.source}"
            )

    def test_clean_package_not_in_warnings(self) -> None:
        """Test that no-warning package doesn't appear in results."""
        warning_attrs = [w.attr for w in self.warnings]
        assert not any("no-warning" in attr for attr in warning_attrs), (
            f"'no-warning' should not appear in warnings: {warning_attrs}"
        )

    def test_input_mapping_contains_myinput(self) -> None:
        """Test that input mapping includes our subflake input."""
        input_names = list(self.input_mapping.values())
        assert "myinput" in input_names, (
            f"Expected 'myinput' in input mapping, got: {self.input_mapping}"
        )

    def test_source_paths_resolved(self) -> None:
        """Test that source locations don't contain raw store paths."""
        for w in self.warnings:
            assert "/nix/store/" not in w.source, (
                f"Expected resolved path, got store path: {w.source}\n"
                f"Input mapping: {self.input_mapping}"
            )


class TestParseNixEvalOutput(unittest.TestCase):
    """Unit tests for parse_nix_eval_output function."""

    def test_builtins_warn_extracts_source_with_line(self) -> None:
        """Test parsing builtins.warn error extracts source with line number."""
        lines = [
            "evaluation warning: test warning message",
            '{"attr":"x86_64-linux.foo","error":"error: evaluation aborted\\n       at /nix/store/abc123-source/flake.nix:25:7:\\n           24|       with-warning = builtins.warn\\n           25|         name = \\"test-pkg\\";\\n              |       ^"}',
        ]
        input_mapping = {"/nix/store/abc123-source": "."}

        results = [w for w in parse_nix_eval_output(lines, input_mapping) if w]
        assert len(results) == 1
        w = results[0]
        assert w.attr == "x86_64-linux.foo"
        assert w.warning_type == "test warning message"
        assert w.source == "flake.nix:25"
        assert w.option_path == ""

    def test_nixos_module_warning_extracts_option_path(self) -> None:
        """Test parsing NixOS module warning extracts option path."""
        error = (
            "error: aborting\\n"
            "       … while evaluating the option `system.build.toplevel':\\n"
            "       … while evaluating the option `warnings':\\n"
            "       … while evaluating the option `systemd.services.phantun.serviceConfig':\\n"
            "       … while evaluating definitions from `/nix/store/abc123-source/nixosModules/phantun':"
        )
        lines = [
            "evaluation warning: 'system' has been renamed",
            f'{{"attr":"x86_64-linux.nixos-eva","error":"{error}"}}',
        ]
        input_mapping = {"/nix/store/abc123-source": "."}

        results = [w for w in parse_nix_eval_output(lines, input_mapping) if w]
        assert len(results) == 1
        w = results[0]
        assert w.attr == "x86_64-linux.nixos-eva"
        assert w.warning_type == "'system' has been renamed"
        assert w.source == "nixosModules/phantun"
        assert w.option_path == "systemd.services.phantun.serviceConfig"

    def test_successful_eval_yields_none(self) -> None:
        """Test that successful evaluations yield None (for counting)."""
        lines = [
            '{"attr":"x86_64-linux.foo","drvPath":"/nix/store/..."}',
        ]
        results = list(parse_nix_eval_output(lines, {}))
        assert len(results) == 1
        assert results[0] is None

    def test_warning_line_correlates_with_next_error(self) -> None:
        """Test that warning message correlates with the following error JSON."""
        lines = [
            '{"attr":"clean","drvPath":"/nix/store/..."}',
            "evaluation warning: first warning",
            '{"attr":"first","error":"error: at /nix/store/src/a.nix:1:1:"}',
            "evaluation warning: second warning",
            '{"attr":"second","error":"error: at /nix/store/src/b.nix:2:2:"}',
        ]
        input_mapping = {"/nix/store/src": "."}

        results = [w for w in parse_nix_eval_output(lines, input_mapping) if w]
        assert len(results) == 2
        assert results[0].attr == "first"
        assert results[0].warning_type == "first warning"
        assert results[1].attr == "second"
        assert results[1].warning_type == "second warning"

    def test_skips_non_json_lines(self) -> None:
        """Test that non-JSON lines (like nix warnings) are skipped."""
        lines = [
            "warning: unknown setting 'foo'",
            "Using saved setting for 'bar'",
            '{"attr":"x86_64-linux.pkg","drvPath":"/nix/store/..."}',
        ]
        results = list(parse_nix_eval_output(lines, {}))
        assert len(results) == 1
        assert results[0] is None


if __name__ == "__main__":
    unittest.main()
