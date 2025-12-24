"""
Extract evaluation warnings from Nix flakes using nix-eval-jobs.

Runs nix-eval-jobs with --option abort-on-warn true and parses output
to extract warnings with their source locations.
"""

import argparse
import json
import re
import subprocess
import sys
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
from typing import Any


@dataclass
class EvalWarning:
    """Represents an evaluation warning from nix-eval-jobs."""

    attr: str
    warning_type: str
    source: str
    option_path: str = ""


def strip_ansi(text: str) -> str:
    """Remove ANSI escape codes from text."""
    return re.compile(r"\x1b\[[0-9;]*m").sub("", text)


def get_flake_input_paths(flake_ref: str) -> dict[str, str]:
    """Get mapping from store paths to flake input names."""
    flake_path = flake_ref.split("#")[0]

    try:
        result = subprocess.run(
            [
                "nix",
                "--extra-experimental-features",
                "nix-command flakes",
                "flake",
                "archive",
                "--json",
                flake_path,
            ],
            capture_output=True,
            text=True,
            check=True,
        )
        data = json.loads(result.stdout)

        mapping: dict[str, str] = {}
        if "path" in data:
            mapping[data["path"]] = "."

        def collect_inputs(inputs: dict[str, Any], prefix: str = "") -> None:
            for name, info in inputs.items():
                input_name = f"{prefix}{name}" if prefix else name
                if "path" in info:
                    mapping[info["path"]] = input_name
                if "inputs" in info:
                    collect_inputs(info["inputs"], f"{input_name}/")

        if "inputs" in data:
            collect_inputs(data["inputs"])
    except (subprocess.CalledProcessError, json.JSONDecodeError, KeyError):
        return {}
    else:
        return mapping


def resolve_store_path(filepath: str, input_mapping: dict[str, str]) -> str:
    """Resolve a nix store path to a flake input name if possible."""
    for store_path, input_name in input_mapping.items():
        if filepath.startswith(store_path):
            relative = filepath[len(store_path) :].lstrip("/")
            if input_name == ".":
                return relative
            return f"{input_name}/{relative}"
    return filepath


def extract_error_message(error_text: str) -> str:
    """Extract error/warning message from error text.

    Handles evaluation errors where there's no 'evaluation warning:' prefix.
    Looks for the main error message after the stack trace.
    """
    # Look for "error: <message>" pattern - get the last one (most specific)
    matches = re.findall(r"error:\s*(.+?)(?:\n|$)", error_text)
    for match in reversed(matches):
        msg = match.strip()
        # Skip generic messages like "aborting" or stack trace fragments
        if (
            msg
            and msg not in ("aborting", "evaluation aborted")
            and not msg.startswith("…")
        ):
            return msg

    return "unknown"


def extract_option_path(error_text: str) -> str:
    """Extract the option path being evaluated from error stack trace.

    Looks for patterns like: while evaluating the option `foo.bar.baz':
    Returns the last (most specific) option path found.
    """
    matches: list[str] = re.findall(
        r"while evaluating the option `([^']+)'", error_text
    )
    if matches:
        return matches[-1]  # Return the most specific (last) option
    return ""


def extract_source(error_text: str, input_mapping: dict[str, str]) -> str:
    """Extract source location from error stack trace.

    Tries two patterns:
    1. 'at /path/file.nix:line:col:' - direct builtins.warn calls
    2. 'definitions from `/nix/store/...':' - NixOS module warnings
    """
    # Try direct "at path:line:col:" pattern first (for builtins.warn)
    match = re.search(r"at (/[^:]+\.nix):(\d+):\d+:", error_text)
    if match:
        filepath = resolve_store_path(match.group(1), input_mapping)
        return f"{filepath}:{match.group(2)}"

    # Fall back to "definitions from" patterns (for NixOS module warnings)
    matches = re.findall(r"definitions from `([^']+)'", error_text)
    for path in reversed(matches):
        clean_path = path.split(",")[0]  # Remove ", via option..." suffix
        resolved = resolve_store_path(clean_path, input_mapping)
        if resolved != clean_path:  # Successfully resolved
            return resolved

    return ""


def parse_nix_eval_output(
    lines: Iterable[str], input_mapping: dict[str, str]
) -> Iterator[EvalWarning | None]:
    """Parse nix-eval-jobs output lines and yield warnings.

    Args:
        lines: Iterable of output lines from nix-eval-jobs
        input_mapping: Mapping from store paths to flake input names

    Yields:
        EvalWarning for lines with errors, None for JSON lines without errors
        (to allow callers to count processed attributes)
    """
    last_warning: str | None = None

    for raw_line in lines:
        line = raw_line.strip()
        if not line:
            continue

        if line.startswith("evaluation warning:"):
            last_warning = line[len("evaluation warning:") :].strip()
            continue

        if line.startswith("{"):
            try:
                data = json.loads(line)
                if "error" in data:
                    error_text = strip_ansi(data["error"])
                    warning_type = last_warning or extract_error_message(error_text)
                    yield EvalWarning(
                        attr=data.get("attr", "unknown"),
                        warning_type=warning_type,
                        source=extract_source(error_text, input_mapping),
                        option_path=extract_option_path(error_text),
                    )
                    last_warning = None
                else:
                    yield None  # Successful eval, no warning
            except json.JSONDecodeError:
                pass


def run_nix_eval_jobs(
    flake_ref: str, input_mapping: dict[str, str], show_progress: bool = False
) -> tuple[list[EvalWarning], int]:
    """Run nix-eval-jobs and return warnings parsed from streamed output."""
    cmd = [
        "nix-eval-jobs",
        "--workers",
        "1",
        "--option",
        "extra-experimental-features",
        "nix-command flakes",
        "--option",
        "abort-on-warn",
        "true",
        "--force-recurse",
        "--no-instantiate",
        "--flake",
        flake_ref,
    ]

    with subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True
    ) as proc:
        assert proc.stdout is not None

        warnings: list[EvalWarning] = []

        for count, result in enumerate(
            parse_nix_eval_output(proc.stdout, input_mapping), start=1
        ):
            if result is not None:
                warnings.append(result)

            if show_progress:
                status = f"\r\x1b[KEvaluating... {count} attributes"
                if warnings:
                    status += f" ({len(warnings)} with warnings)"
                print(status, end="", file=sys.stderr, flush=True)

        if show_progress:
            print("\r\x1b[K", end="", file=sys.stderr, flush=True)

        return warnings, proc.wait()


EXIT_OK = 0
EXIT_WARNINGS = 1
EXIT_ERROR = 2


def main() -> int:
    """Run nix-eval-jobs and extract evaluation warnings.

    Exit codes: 0=success, 1=warnings found, 2=error
    """
    parser = argparse.ArgumentParser(
        description="Extract evaluation warnings from Nix flakes",
        epilog="Exit codes: 0=success, 1=warnings found, 2=error",
    )
    parser.add_argument(
        "flake_ref",
        help="Flake reference (e.g., '.#checks' or '/path/to/flake#packages')",
    )
    parser.add_argument("--json", action="store_true", help="Output as JSON")

    args = parser.parse_args()

    input_mapping = get_flake_input_paths(args.flake_ref)

    try:
        warnings, _ = run_nix_eval_jobs(
            args.flake_ref, input_mapping, show_progress=sys.stderr.isatty()
        )
    except FileNotFoundError:
        print("Error: nix-eval-jobs not found in PATH", file=sys.stderr)
        return EXIT_ERROR

    if not warnings:
        print("No evaluation warnings found.", file=sys.stderr)
        return EXIT_OK

    if args.json:
        print(json.dumps([vars(w) for w in warnings], indent=2))
    else:
        # Group by warning type
        by_type: dict[str, list[EvalWarning]] = {}
        for w in warnings:
            by_type.setdefault(w.warning_type, []).append(w)

        print(f"\nFound {len(warnings)} warning(s):\n")
        for warning_type, type_warnings in by_type.items():
            print(f"## {warning_type}\n")
            for w in type_warnings:
                print(f"  - {w.attr}")
                if w.source:
                    print(f"    {w.source}")
                if w.option_path:
                    print(f"    option: {w.option_path}")
            print()

    return EXIT_WARNINGS


if __name__ == "__main__":
    sys.exit(main())
