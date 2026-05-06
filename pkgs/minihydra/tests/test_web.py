"""Web layer: smoke-test the FastAPI app against an in-process sqlite db."""

from __future__ import annotations

import os
from pathlib import Path

from fastapi.testclient import TestClient
from minihydra import db


def _seed(tmp_path: Path) -> Path:
    p = tmp_path / "m.db"
    conn = db.connect(p)
    rid = db.create_run(
        conn,
        flake_ref="repo",
        attr="legacyPackages.x86_64-linux",
        base_rev="HEAD~1",
        head_rev="HEAD",
        base_resolved="a" * 40,
        head_resolved="b" * 40,
    )
    db.insert_job(
        conn,
        run_id=rid,
        side="base",
        record={
            "attr": "hello",
            "drvPath": "/nix/store/x.drv",
            "system": "x86_64-linux",
        },
    )
    db.insert_job(
        conn,
        run_id=rid,
        side="head",
        record={
            "attr": "hello",
            "drvPath": "/nix/store/y.drv",
            "system": "x86_64-linux",
        },
    )
    db.update_progress(conn, rid, "eval-head", current=1, total=1, message="done")
    db.set_run_status(conn, rid, "done")
    conn.close()
    return p


def test_routes(tmp_path: Path) -> None:
    db_path = _seed(tmp_path)
    os.environ["MINIHYDRA_DB"] = str(db_path)
    # import after env var so the app picks it up if it ever caches
    from minihydra.web import app

    client = TestClient(app)
    r = client.get("/")
    assert r.status_code == 200
    assert "runs" in r.text

    r = client.get("/run/1")
    assert r.status_code == 200
    assert "changed" in r.text or "diff" in r.text

    r = client.get("/run/1/progress")
    assert r.status_code == 200
    assert "eval-head" in r.text

    r = client.get("/run/1/section/changed")
    assert r.status_code == 200
    assert "hello" in r.text

    r = client.get("/run/999")
    assert r.status_code == 404

    # JSON API parity with the human routes.
    r = client.get("/api/runs")
    assert r.status_code == 200
    assert isinstance(r.json(), list)
    assert r.json()[0]["id"] == 1

    r = client.get("/api/run/1")
    assert r.status_code == 200
    body = r.json()
    assert body["summary"]["changed"] == 1
    assert body["counts"] == {"base": 1, "head": 1}

    r = client.get("/api/run/1/diff")
    assert r.status_code == 200
    assert r.json()["changed"][0]["attr"] == "hello"

    r = client.get("/api/run/1/section/changed")
    assert r.status_code == 200
    assert len(r.json()["entries"]) == 1

    r = client.get("/api/run/1/section/bogus")
    assert r.status_code == 404


def test_log_endpoint_path_traversal(tmp_path: Path) -> None:
    """A poisoned build_log_path must not exfiltrate files outside the root."""
    db_path = _seed(tmp_path)
    log_root = tmp_path / "logs"
    log_root.mkdir()
    legit = log_root / "1.log"
    legit.write_text("hello-log")
    secret = tmp_path / "secret.txt"
    secret.write_text("TOPSECRET")

    from minihydra import db as dbmod

    conn = dbmod.connect(db_path)
    # job id 1 = base side hello, id 2 = head side hello
    dbmod.update_build_status(conn, [2], "ok", log_path=str(legit))
    dbmod.update_build_status(conn, [1], "ok", log_path=str(secret))
    conn.close()

    os.environ["MINIHYDRA_DB"] = str(db_path)
    os.environ["MINIHYDRA_LOG_ROOT"] = str(log_root)
    from minihydra.web import app

    client = TestClient(app)

    # Legit path inside the root: 200 + content.
    r = client.get("/run/1/log/2")
    assert r.status_code == 200
    assert r.text == "hello-log"

    # Path outside the root: 403, never the file contents.
    r = client.get("/run/1/log/1")
    assert r.status_code == 403
    assert "TOPSECRET" not in r.text

    # With no MINIHYDRA_LOG_ROOT, the endpoint refuses unconditionally.
    del os.environ["MINIHYDRA_LOG_ROOT"]
    r = client.get("/run/1/log/2")
    assert r.status_code == 403


def test_template_filters() -> None:
    """The drv-shortening + first-lines filters underpin the readable UI."""
    from minihydra.web import _datetime, _drv_short, _first_lines

    # Real-shaped 32-char hash gets stripped and .drv suffix removed.
    assert (
        _drv_short("/nix/store/abcdefghijklmnopqrstuvwxyz012345-hello-1.0.drv")
        == "hello-1.0"
    )
    # Non-conforming hash is left alone (defensive: better to show too much
    # than to silently mangle a path).
    assert _drv_short("/nix/store/short-fake.drv") == "/nix/store/short-fake"
    assert _drv_short(None) == ""
    assert _drv_short("") == ""

    # first_lines truncates and indicates how many lines were dropped.
    long = "\n".join(f"line {i}" for i in range(10))
    assert _first_lines(long, 3).startswith("line 0\nline 1\nline 2")
    assert "7 more lines" in _first_lines(long, 3)
    assert _first_lines(None) == ""

    # datetime: no exception on edge values.
    assert _datetime(0).startswith("1970-01-01")
    assert _datetime(None) == ""


def _seed_many(tmp_path: Path, n_unchanged: int = 250, n_changed: int = 5) -> Path:
    """Larger fixture so the pagination/search/cache tests have room to work."""
    p = tmp_path / "big.db"
    conn = db.connect(p)
    rid = db.create_run(
        conn,
        flake_ref="repo",
        attr="legacyPackages.x86_64-linux",
        base_rev="HEAD~1",
        head_rev="HEAD",
        base_resolved="a" * 40,
        head_resolved="b" * 40,
    )
    h = "abcdefghijklmnopqrstuvwxyz012345"
    for i in range(n_unchanged):
        d = f"/nix/store/{h}-pkg{i:04d}-1.0.drv"
        for side in ("base", "head"):
            db.insert_job(
                conn,
                run_id=rid,
                side=side,
                record={
                    "attr": f"unchanged.pkg{i:04d}",
                    "drvPath": d,
                    "system": "x86_64-linux",
                },
            )
    for i in range(n_changed):
        db.insert_job(
            conn,
            run_id=rid,
            side="base",
            record={"attr": f"python.pkg{i}", "drvPath": f"/nix/store/{h}-old{i}.drv"},
        )
        db.insert_job(
            conn,
            run_id=rid,
            side="head",
            record={"attr": f"python.pkg{i}", "drvPath": f"/nix/store/{h}-new{i}.drv"},
        )
    db.update_progress(
        conn, rid, "eval-head", current=n_unchanged, total=n_unchanged, message="done"
    )
    db.set_run_status(conn, rid, "done")
    conn.close()
    return p


def test_pagination_limits_section_body(tmp_path: Path) -> None:
    """Sections must paginate so a 30k-row unchanged bucket doesn't blow up
    the page. The load-more button is the contract; verify it shows up."""
    db_path = _seed_many(tmp_path, n_unchanged=250)
    os.environ["MINIHYDRA_DB"] = str(db_path)
    from minihydra import web
    from minihydra.web import app

    web._DIFF_CACHE.clear()  # noqa: SLF001
    client = TestClient(app)

    # Default limit is 100; with 250 unchanged rows we expect a load-more.
    r = client.get("/run/1/section/unchanged")
    assert r.status_code == 200
    assert "load-more" in r.text
    assert "100 of 250" in r.text
    # Bumping limit shrinks the remaining count.
    r = client.get("/run/1/section/unchanged?limit=200")
    assert "200 of 250" in r.text
    # Limit beyond total: no button, all-shown footer.
    r = client.get("/run/1/section/unchanged?limit=1000")
    assert "load-more" not in r.text
    assert "all 250 shown" in r.text


def test_search_filters_attrs(tmp_path: Path) -> None:
    """The search endpoint must narrow each section to attrs containing q,
    case-insensitively, and reuse the diff-area template."""
    db_path = _seed_many(tmp_path)
    os.environ["MINIHYDRA_DB"] = str(db_path)
    from minihydra import web
    from minihydra.web import app

    web._DIFF_CACHE.clear()  # noqa: SLF001
    client = TestClient(app)

    r = client.get("/run/1/search?q=python")
    assert r.status_code == 200
    # The 5 changed.python.* match; no unchanged should leak through.
    assert "python.pkg0" in r.text
    assert "unchanged.pkg0000" not in r.text
    # Empty match still renders the area with a friendly message.
    r = client.get("/run/1/search?q=zzzzzzzznope")
    assert "no attributes match" in r.text

    # Case-insensitive.
    r = client.get("/run/1/search?q=PYTHON")
    assert "python.pkg0" in r.text


def test_diff_cache_invalidates_on_progress_update(tmp_path: Path) -> None:
    """Cache key uses the latest progress.updated_at so a still-running
    eval picks up new rows. Stale cache would mean the UI shows old data."""
    db_path = _seed_many(tmp_path, n_unchanged=10)
    os.environ["MINIHYDRA_DB"] = str(db_path)
    from minihydra import web

    web._DIFF_CACHE.clear()  # noqa: SLF001
    conn = db.connect(db_path)
    d1 = web._get_diff(conn, 1)  # noqa: SLF001
    assert len(d1.unchanged) == 10
    assert 1 in web._DIFF_CACHE  # noqa: SLF001

    # Insert a new attr and bump progress; cache should re-compute.
    db.insert_job(
        conn,
        run_id=1,
        side="head",
        record={"attr": "brandnew", "drvPath": "/nix/store/x-new.drv"},
    )
    db.update_progress(conn, 1, "eval-head", current=11, total=11, message="more")
    d2 = web._get_diff(conn, 1)  # noqa: SLF001
    assert any(e.attr == "brandnew" for e in d2.added)


def test_inline_log_endpoint_tails(tmp_path: Path) -> None:
    """Inline log fragment must tail to keep diff pages small."""
    db_path = _seed(tmp_path)
    log_root = tmp_path / "logs"
    log_root.mkdir()
    log = log_root / "big.log"
    log.write_text("\n".join(f"line {i}" for i in range(1000)))

    conn = db.connect(db_path)
    db.update_build_status(conn, [2], "ok", log_path=str(log))
    conn.close()

    os.environ["MINIHYDRA_DB"] = str(db_path)
    os.environ["MINIHYDRA_LOG_ROOT"] = str(log_root)
    from minihydra.web import app

    client = TestClient(app)
    r = client.get("/run/1/log/2/inline")
    assert r.status_code == 200
    # Tail caps at 400 lines; a 1000-line log triggers the truncation hint.
    assert "line 999" in r.text
    assert "line 0\n" not in r.text
    assert "showing last 400 of 1000" in r.text
    del os.environ["MINIHYDRA_LOG_ROOT"]
