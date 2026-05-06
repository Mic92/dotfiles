"""SQLite layer.

Single-file database keyed by run id. Schema is intentionally narrow: a row
per (run, side, system, attr). Build status piggy-backs on the same row so we
can join in the UI without extra tables.
"""

from __future__ import annotations

import json
import sqlite3
import time
from collections.abc import Iterable, Iterator
from contextlib import contextmanager
from pathlib import Path
from typing import Any

SCHEMA = """
CREATE TABLE IF NOT EXISTS runs (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at  REAL    NOT NULL,
    flake_ref   TEXT    NOT NULL,
    attr        TEXT    NOT NULL,
    base_rev    TEXT    NOT NULL,
    head_rev    TEXT    NOT NULL,
    base_resolved TEXT  NOT NULL,
    head_resolved TEXT  NOT NULL,
    status      TEXT    NOT NULL DEFAULT 'pending',
    note        TEXT
);

CREATE TABLE IF NOT EXISTS jobs (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id      INTEGER NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
    side        TEXT    NOT NULL CHECK (side IN ('base', 'head')),
    attr        TEXT    NOT NULL,
    system      TEXT,
    name        TEXT,
    drv_path    TEXT,
    outputs     TEXT,        -- json: {name: store_path}
    error       TEXT,
    cache_status TEXT,
    raw         TEXT,        -- full nix-eval-jobs json line
    build_status TEXT,        -- null | queued | building | ok | failed | skipped
    build_log_path TEXT
);

CREATE INDEX IF NOT EXISTS idx_jobs_run_side ON jobs(run_id, side);
CREATE INDEX IF NOT EXISTS idx_jobs_run_attr ON jobs(run_id, attr);
CREATE INDEX IF NOT EXISTS idx_jobs_drv ON jobs(run_id, drv_path);

CREATE TABLE IF NOT EXISTS progress (
    run_id      INTEGER NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
    phase       TEXT    NOT NULL,
    current     INTEGER NOT NULL DEFAULT 0,
    total       INTEGER NOT NULL DEFAULT 0,
    message     TEXT,
    updated_at  REAL    NOT NULL,
    PRIMARY KEY (run_id, phase)
);
"""


def connect(path: Path) -> sqlite3.Connection:
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path, isolation_level=None, timeout=30)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA foreign_keys=ON")
    conn.executescript(SCHEMA)
    return conn


@contextmanager
def transaction(conn: sqlite3.Connection) -> Iterator[None]:
    conn.execute("BEGIN")
    try:
        yield
    except Exception:
        conn.execute("ROLLBACK")
        raise
    else:
        conn.execute("COMMIT")


def create_run(
    conn: sqlite3.Connection,
    *,
    flake_ref: str,
    attr: str,
    base_rev: str,
    head_rev: str,
    base_resolved: str,
    head_resolved: str,
) -> int:
    cur = conn.execute(
        """INSERT INTO runs
           (created_at, flake_ref, attr, base_rev, head_rev,
            base_resolved, head_resolved, status)
           VALUES (?, ?, ?, ?, ?, ?, ?, 'running')""",
        (
            time.time(),
            flake_ref,
            attr,
            base_rev,
            head_rev,
            base_resolved,
            head_resolved,
        ),
    )
    rid = cur.lastrowid
    assert rid is not None
    return rid


def set_run_status(conn: sqlite3.Connection, run_id: int, status: str) -> None:
    conn.execute("UPDATE runs SET status = ? WHERE id = ?", (status, run_id))


def insert_job(
    conn: sqlite3.Connection,
    *,
    run_id: int,
    side: str,
    record: dict[str, Any],
) -> None:
    attr = record.get("attr") or record.get("attrPath") or ""
    if isinstance(attr, list):
        attr = ".".join(attr)
    outputs = record.get("outputs")
    conn.execute(
        """INSERT INTO jobs
           (run_id, side, attr, system, name, drv_path, outputs, error,
            cache_status, raw)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
        (
            run_id,
            side,
            attr,
            record.get("system"),
            record.get("name"),
            record.get("drvPath"),
            json.dumps(outputs) if outputs is not None else None,
            record.get("error"),
            json.dumps(record.get("cacheStatus"))
            if record.get("cacheStatus") is not None
            else None,
            json.dumps(record),
        ),
    )


def update_progress(
    conn: sqlite3.Connection,
    run_id: int,
    phase: str,
    *,
    current: int = 0,
    total: int = 0,
    message: str | None = None,
) -> None:
    conn.execute(
        """INSERT INTO progress (run_id, phase, current, total, message, updated_at)
           VALUES (?, ?, ?, ?, ?, ?)
           ON CONFLICT(run_id, phase) DO UPDATE SET
              current = excluded.current,
              total = excluded.total,
              message = excluded.message,
              updated_at = excluded.updated_at""",
        (run_id, phase, current, total, message, time.time()),
    )


def runs(conn: sqlite3.Connection) -> list[sqlite3.Row]:
    return list(conn.execute("SELECT * FROM runs ORDER BY created_at DESC").fetchall())


def get_run(conn: sqlite3.Connection, run_id: int) -> sqlite3.Row | None:
    return conn.execute("SELECT * FROM runs WHERE id = ?", (run_id,)).fetchone()


def latest_run(conn: sqlite3.Connection) -> sqlite3.Row | None:
    return conn.execute(
        "SELECT * FROM runs ORDER BY created_at DESC LIMIT 1"
    ).fetchone()


def copy_side_jobs(
    conn: sqlite3.Connection,
    src_run_id: int,
    dst_run_id: int,
    side: str,
) -> int:
    """Copy every job row from one run/side onto another run/side.

    Used by ``minihydra rerun`` to reuse a base eval across iterations
    instead of re-running ``nix-eval-jobs`` against the same base revision.
    Returns the number of rows copied.
    """
    # Copy every column except `id` (autoincremented) and `run_id` (the
    # whole point). Including build_status / build_log_path means a future
    # caller that copies the head side keeps build results intact, instead
    # of silently losing them.
    cur = conn.execute(
        """INSERT INTO jobs
             (run_id, side, attr, system, name, drv_path, outputs,
              error, cache_status, raw, build_status, build_log_path)
           SELECT ?, side, attr, system, name, drv_path, outputs,
                  error, cache_status, raw, build_status, build_log_path
             FROM jobs
            WHERE run_id = ? AND side = ?""",
        (dst_run_id, src_run_id, side),
    )
    return cur.rowcount or 0


def progress_for(conn: sqlite3.Connection, run_id: int) -> list[sqlite3.Row]:
    return list(
        conn.execute(
            "SELECT * FROM progress WHERE run_id = ? ORDER BY updated_at",
            (run_id,),
        ).fetchall()
    )


def jobs_for(
    conn: sqlite3.Connection,
    run_id: int,
    *,
    side: str | None = None,
) -> list[sqlite3.Row]:
    if side:
        return list(
            conn.execute(
                "SELECT * FROM jobs WHERE run_id = ? AND side = ? ORDER BY attr",
                (run_id, side),
            ).fetchall()
        )
    return list(
        conn.execute(
            "SELECT * FROM jobs WHERE run_id = ? ORDER BY side, attr",
            (run_id,),
        ).fetchall()
    )


def update_build_status(
    conn: sqlite3.Connection,
    job_ids: Iterable[int],
    status: str,
    *,
    log_path: str | None = None,
) -> None:
    for jid in job_ids:
        conn.execute(
            "UPDATE jobs SET build_status = ?, build_log_path = COALESCE(?, build_log_path) WHERE id = ?",
            (status, log_path, jid),
        )
