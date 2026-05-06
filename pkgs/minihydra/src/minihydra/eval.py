"""Drive nix-eval-jobs against a flake checkout and stream results to sqlite."""

from __future__ import annotations

import json
import shutil
import sqlite3
import subprocess
import sys
import tempfile
from collections.abc import Callable, Iterator
from contextlib import contextmanager
from pathlib import Path

from minihydra import db


def git(repo: Path, *args: str) -> str:
    res = subprocess.run(
        ["git", "-C", str(repo), *args],
        check=True,
        capture_output=True,
        text=True,
    )
    return res.stdout.strip()


def resolve_rev(repo: Path, rev: str) -> str:
    return git(repo, "rev-parse", rev)


def find_repo_root(start: Path) -> Path:
    return Path(git(start, "rev-parse", "--show-toplevel"))


@contextmanager
def worktree_at(repo: Path, rev: str) -> Iterator[Path]:
    """Create a detached git worktree at *rev* and clean it up afterwards.

    Using `git worktree` instead of stash/checkout means concurrent edits in
    the user's main checkout are unaffected.
    """
    tmp = Path(tempfile.mkdtemp(prefix="minihydra-wt-"))
    wt = tmp / "wt"
    try:
        # `--` sentinel keeps a hostile rev like `--upload-pack=...` from
        # being interpreted as a git option. `git worktree add` takes
        # `<path> <commit-ish>` as positionals after `--`.
        subprocess.run(
            [
                "git",
                "-C",
                str(repo),
                "worktree",
                "add",
                "--detach",
                "--",
                str(wt),
                rev,
            ],
            check=True,
        )
        yield wt
    finally:
        subprocess.run(
            ["git", "-C", str(repo), "worktree", "remove", "--force", str(wt)],
            check=False,
        )
        shutil.rmtree(tmp, ignore_errors=True)


def run_nix_eval_jobs(
    flake_ref: str,
    attr: str,
    *,
    workers: int = 4,
    extra_args: list[str] | None = None,
    on_record: Callable[[dict[str, object]], None],
    on_progress: Callable[[int], None] | None = None,
) -> int:
    """Stream `nix-eval-jobs` JSONL output, invoking *on_record* per attribute.

    Returns the count of records seen. Exit code is checked so callers can
    distinguish a tool crash from per-attribute eval errors (which are
    reported in-band via the `error` field).
    """
    cmd = [
        "nix-eval-jobs",
        "--flake",
        f"{flake_ref}#{attr}" if attr else flake_ref,
        "--workers",
        str(workers),
        "--force-recurse",
        "--no-instantiate",
        *(extra_args or []),
    ]
    seen = 0
    with subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        text=True,
        bufsize=1,
    ) as proc:
        assert proc.stdout is not None
        for raw_line in proc.stdout:
            line = raw_line.strip()
            if not line:
                continue
            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                continue
            on_record(record)
            seen += 1
            if on_progress is not None:
                on_progress(seen)
        rc = proc.wait()
    if rc != 0:
        msg = f"nix-eval-jobs exited with status {rc}"
        raise RuntimeError(msg)
    return seen


def evaluate_side(
    conn: sqlite3.Connection,
    *,
    run_id: int,
    side: str,
    flake_ref: str,
    attr: str,
    workers: int,
    on_progress: Callable[[int], None] | None = None,
) -> int:
    """Evaluate one side and persist all rows in a single transaction."""

    def store(record: dict[str, object]) -> None:
        db.insert_job(conn, run_id=run_id, side=side, record=record)

    with db.transaction(conn):
        return run_nix_eval_jobs(
            flake_ref,
            attr,
            workers=workers,
            on_record=store,
            on_progress=on_progress,
        )
