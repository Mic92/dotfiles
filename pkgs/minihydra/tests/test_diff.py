"""Diff classification: stable input -> deterministic bucket placement."""

from __future__ import annotations

import sqlite3

from minihydra import db, diff


def _setup() -> tuple[sqlite3.Connection, int]:
    conn = sqlite3.connect(":memory:")
    conn.row_factory = sqlite3.Row
    conn.executescript(db.SCHEMA)
    rid = db.create_run(
        conn,
        flake_ref="x",
        attr="legacyPackages.x86_64-linux",
        base_rev="HEAD~1",
        head_rev="HEAD",
        base_resolved="a" * 40,
        head_resolved="b" * 40,
    )
    return conn, rid


def _ins(conn, rid, side, attr, *, drv=None, error=None):
    db.insert_job(
        conn,
        run_id=rid,
        side=side,
        record={"attr": attr, "drvPath": drv, "error": error, "system": "x86_64-linux"},
    )


def test_diff_buckets() -> None:
    conn, rid = _setup()
    # unchanged: same drv on both sides
    _ins(conn, rid, "base", "hello", drv="/nix/store/aaa-hello.drv")
    _ins(conn, rid, "head", "hello", drv="/nix/store/aaa-hello.drv")
    # changed: drv differs
    _ins(conn, rid, "base", "git", drv="/nix/store/bbb-git.drv")
    _ins(conn, rid, "head", "git", drv="/nix/store/ccc-git.drv")
    # added
    _ins(conn, rid, "head", "newpkg", drv="/nix/store/ddd-new.drv")
    # removed
    _ins(conn, rid, "base", "oldpkg", drv="/nix/store/eee-old.drv")
    # newly_broken
    _ins(conn, rid, "base", "broke", drv="/nix/store/fff.drv")
    _ins(conn, rid, "head", "broke", error="boom")
    # newly_fixed
    _ins(conn, rid, "base", "fixed", error="boom")
    _ins(conn, rid, "head", "fixed", drv="/nix/store/ggg.drv")
    # still_broken
    _ins(conn, rid, "base", "stuck", error="x")
    _ins(conn, rid, "head", "stuck", error="x")

    base = db.jobs_for(conn, rid, side="base")
    head = db.jobs_for(conn, rid, side="head")
    d = diff.compute(base, head)
    s = d.summary()
    assert s == {
        "added": 1,
        "removed": 1,
        "changed": 1,
        "unchanged": 1,
        "newly_broken": 1,
        "newly_fixed": 1,
        "still_broken": 1,
    }
    assert d.added[0].attr == "newpkg"
    assert d.removed[0].attr == "oldpkg"
    assert d.newly_broken[0].attr == "broke"


def test_progress_upsert() -> None:
    conn, rid = _setup()
    db.update_progress(conn, rid, "eval-base", current=1, total=10, message="a")
    db.update_progress(conn, rid, "eval-base", current=5, total=10, message="b")
    rows = db.progress_for(conn, rid)
    assert len(rows) == 1
    assert rows[0]["current"] == 5
    assert rows[0]["message"] == "b"


def test_copy_side_jobs_round_trip() -> None:
    """`minihydra rerun` reuses a prior run's base side; verify the copy
    helper preserves attrs, drv paths, and errors so the diff is identical
    against the new head."""
    conn, rid = _setup()
    _ins(conn, rid, "base", "hello", drv="/nix/store/aaa.drv")
    _ins(conn, rid, "base", "git", drv="/nix/store/bbb.drv")
    _ins(conn, rid, "base", "broken", error="boom")
    # Different side and a head row should not bleed across.
    _ins(conn, rid, "head", "hello", drv="/nix/store/aaa.drv")

    rid2 = db.create_run(
        conn,
        flake_ref="x",
        attr="legacyPackages.x86_64-linux",
        base_rev="HEAD~1",
        head_rev="HEAD",
        base_resolved="a" * 40,
        head_resolved="c" * 40,
    )
    n = db.copy_side_jobs(conn, rid, rid2, "base")
    assert n == 3

    copied = db.jobs_for(conn, rid2, side="base")
    assert {(r["attr"], r["drv_path"], r["error"]) for r in copied} == {
        ("hello", "/nix/store/aaa.drv", None),
        ("git", "/nix/store/bbb.drv", None),
        ("broken", None, "boom"),
    }
    # Head-side rows of run 1 are untouched.
    assert db.jobs_for(conn, rid2, side="head") == []


def test_copy_side_jobs_preserves_build_columns() -> None:
    """Even though `rerun` only copies the base side today, the helper must
    keep build_status/build_log_path so future callers that copy head don't
    silently lose build results."""
    conn, rid = _setup()
    _ins(conn, rid, "head", "foo", drv="/nix/store/foo.drv")
    foo_id = conn.execute("SELECT id FROM jobs WHERE attr='foo'").fetchone()[0]
    db.update_build_status(
        conn, [foo_id], "failed", log_path="/var/log/foo.log"
    )

    rid2 = db.create_run(
        conn, flake_ref="x", attr="a",
        base_rev="b", head_rev="h",
        base_resolved="a" * 40, head_resolved="b" * 40,
    )
    db.copy_side_jobs(conn, rid, rid2, "head")
    row = db.jobs_for(conn, rid2, side="head")[0]
    assert row["build_status"] == "failed"
    assert row["build_log_path"] == "/var/log/foo.log"


def test_latest_run_orders_by_created_at() -> None:
    conn, _ = _setup()
    rid2 = db.create_run(
        conn,
        flake_ref="x",
        attr="a",
        base_rev="b",
        head_rev="h",
        base_resolved="b" * 40,
        head_resolved="h" * 40,
    )
    latest = db.latest_run(conn)
    assert latest is not None
    assert latest["id"] == rid2
