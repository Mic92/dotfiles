"""Local build driver: realise drv paths sequentially, capture per-job logs."""

from __future__ import annotations

import shutil
import sqlite3
import subprocess
from collections.abc import Callable
from pathlib import Path

from minihydra import db


def build_jobs(
    conn: sqlite3.Connection,
    run_id: int,
    *,
    side: str = "head",
    log_dir: Path,
    on_progress: Callable[[int, int, str], None] | None = None,
) -> None:
    """Realise every drv from *side* that hasn't been built yet.

    Sequential by design (the user picked sequential concurrency). We rely on
    nix's own daemon-side scheduling for parallelism within a single drv.
    """
    log_dir.mkdir(parents=True, exist_ok=True)
    rows = [
        r
        for r in db.jobs_for(conn, run_id, side=side)
        if r["drv_path"] and not r["error"]
    ]
    total = len(rows)
    for i, row in enumerate(rows, start=1):
        drv = row["drv_path"]
        attr = row["attr"]
        if on_progress is not None:
            on_progress(i, total, attr)
        log_path = log_dir / f"{row['id']}.log"
        with log_path.open("wb") as fh:
            proc = subprocess.run(
                ["nix-store", "--realise", drv],
                stdout=fh,
                stderr=subprocess.STDOUT,
                check=False,
            )
        status = "ok" if proc.returncode == 0 else "failed"
        with db.transaction(conn):
            db.update_build_status(conn, [row["id"]], status, log_path=str(log_path))


def have_nix_store() -> bool:
    return shutil.which("nix-store") is not None
