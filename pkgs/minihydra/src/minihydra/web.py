"""FastAPI + HTMX web UI.

The DB path is read from the MINIHYDRA_DB env var (set by the CLI) so the
ASGI app object stays trivially constructable for tests.
"""

from __future__ import annotations

import datetime as dt
import json
import os
import re
import sqlite3
from dataclasses import asdict
from pathlib import Path
from typing import Any

from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import HTMLResponse, JSONResponse, PlainTextResponse
from fastapi.templating import Jinja2Templates

from minihydra import db, diff

TEMPLATES_DIR = Path(__file__).parent / "templates"
templates = Jinja2Templates(directory=str(TEMPLATES_DIR))


_DRV_HASH_RE = re.compile(r"^/nix/store/[a-z0-9]{32}-")

#: Bucket names in the order the UI presents them: regressions and fixes
#: first, then identity changes, then chronic / unchanged.
SECTIONS = (
    "newly_broken",
    "newly_fixed",
    "changed",
    "added",
    "removed",
    "still_broken",
    "unchanged",
)

#: Sections that are open by default in the run view.
SECTIONS_OPEN_BY_DEFAULT = frozenset(
    {"newly_broken", "newly_fixed", "changed", "added", "removed"}
)

#: How many rows to render per section before requiring the user to click
#: "load more". 100 is plenty to skim a regression set without blowing up
#: page weight on a real nixpkgs eval (~80k unchanged attrs).
DEFAULT_LIMIT = 100


def _drv_short(path: str | None) -> str:
    """Strip /nix/store/<hash>- prefix and trailing .drv for display.

    Keeps the human-meaningful name+version while dropping the hash that
    differs on every rebuild.
    """
    if not path:
        return ""
    return _DRV_HASH_RE.sub("", path).removesuffix(".drv")


def _first_lines(text: str | None, n: int = 4) -> str:
    if not text:
        return ""
    lines = text.splitlines()
    out = "\n".join(lines[:n])
    if len(lines) > n:
        out += f"\n… ({len(lines) - n} more lines)"
    return out


def _datetime(ts: float | None) -> str:
    if ts is None:
        return ""
    return dt.datetime.fromtimestamp(float(ts), tz=dt.UTC).strftime(
        "%Y-%m-%d %H:%M:%S UTC"
    )


templates.env.filters["drv_short"] = _drv_short
templates.env.filters["first_lines"] = _first_lines
templates.env.filters["datetime"] = _datetime
templates.env.globals["SECTIONS"] = SECTIONS
templates.env.globals["SECTIONS_OPEN_BY_DEFAULT"] = SECTIONS_OPEN_BY_DEFAULT
templates.env.globals["DEFAULT_LIMIT"] = DEFAULT_LIMIT

app = FastAPI(title="minihydra")


def _conn() -> sqlite3.Connection:
    path = os.environ.get("MINIHYDRA_DB")
    if not path:
        raise HTTPException(500, "MINIHYDRA_DB not set")
    return db.connect(Path(path))


# ---------------------------------------------------------------------------
# Diff cache
#
# Computing the diff over an 80k-attribute nixpkgs eval takes ~1s. Without a
# cache, every keystroke in the search box and every "load more" click would
# re-run that work. We key on the run id plus the largest progress.updated_at
# timestamp, so a still-running eval invalidates automatically as new attrs
# stream in, but a finished run hits the cache forever (until the process
# restarts). This trades a small amount of memory for a much snappier UI.
# ---------------------------------------------------------------------------

_DIFF_CACHE: dict[int, tuple[float, diff.Diff]] = {}


def _diff_freshness(conn: sqlite3.Connection, run_id: int) -> float:
    row = conn.execute(
        "SELECT COALESCE(MAX(updated_at), 0) FROM progress WHERE run_id = ?",
        (run_id,),
    ).fetchone()
    return float(row[0] or 0.0)


def _get_diff(conn: sqlite3.Connection, run_id: int) -> diff.Diff:
    stamp = _diff_freshness(conn, run_id)
    cached = _DIFF_CACHE.get(run_id)
    if cached and cached[0] == stamp:
        return cached[1]
    base_rows = db.jobs_for(conn, run_id, side="base")
    head_rows = db.jobs_for(conn, run_id, side="head")
    d = diff.compute(base_rows, head_rows)
    _DIFF_CACHE[run_id] = (stamp, d)
    return d


def _filter_diff(d: diff.Diff, q: str) -> diff.Diff:
    """Return a new Diff with each section narrowed to attrs containing *q*.

    Case-insensitive substring match on the attribute path. We rebuild a
    fresh :class:`diff.Diff` rather than mutate so the cache stays clean.
    """
    if not q:
        return d
    needle = q.lower()
    out = diff.Diff()
    for name in SECTIONS:
        setattr(
            out,
            name,
            [e for e in getattr(d, name) if needle in e.attr.lower()],
        )
    return out


# ---------------------------------------------------------------------------
# HTML routes
# ---------------------------------------------------------------------------


@app.get("/", response_class=HTMLResponse)
def index(request: Request) -> HTMLResponse:
    conn = _conn()
    return templates.TemplateResponse(request, "index.html", {"runs": db.runs(conn)})


def _diff_context(
    request: Request,
    run: sqlite3.Row,
    d: diff.Diff,
    q: str,
    limit: int,
) -> dict[str, Any]:
    """Shared template variables for the run page and the search fragment."""
    return {
        "request": request,
        "run": run,
        "diff": d,
        "summary": d.summary(),
        "q": q,
        "limit": limit,
    }


@app.get("/run/{run_id}", response_class=HTMLResponse)
def run_view(
    request: Request,
    run_id: int,
    q: str = "",
    limit: int = DEFAULT_LIMIT,
) -> HTMLResponse:
    conn = _conn()
    run = db.get_run(conn, run_id)
    if not run:
        raise HTTPException(404)
    d = _filter_diff(_get_diff(conn, run_id), q)
    ctx = _diff_context(request, run, d, q, limit)
    ctx["progress"] = db.progress_for(conn, run_id)
    return templates.TemplateResponse(request, "run.html", ctx)


@app.get("/run/{run_id}/search", response_class=HTMLResponse)
def run_search(
    request: Request,
    run_id: int,
    q: str = "",
    limit: int = DEFAULT_LIMIT,
) -> HTMLResponse:
    """HTMX target for the search box: re-renders just the diff area."""
    conn = _conn()
    run = db.get_run(conn, run_id)
    if not run:
        raise HTTPException(404)
    d = _filter_diff(_get_diff(conn, run_id), q)
    return templates.TemplateResponse(
        request, "_diff_area.html", _diff_context(request, run, d, q, limit)
    )


@app.get("/run/{run_id}/section/{name}", response_class=HTMLResponse)
def run_section(
    request: Request,
    run_id: int,
    name: str,
    q: str = "",
    limit: int = DEFAULT_LIMIT,
) -> HTMLResponse:
    """HTMX target for the per-section "load more" button."""
    if name not in SECTIONS:
        raise HTTPException(404, f"unknown section {name}")
    conn = _conn()
    run = db.get_run(conn, run_id)
    if not run:
        raise HTTPException(404)
    d = _filter_diff(_get_diff(conn, run_id), q)
    return templates.TemplateResponse(
        request,
        "_section.html",
        {
            "request": request,
            "run": run,
            "name": name,
            "entries": getattr(d, name),
            "limit": limit,
            "q": q,
        },
    )


@app.get("/run/{run_id}/progress", response_class=HTMLResponse)
def run_progress(request: Request, run_id: int) -> HTMLResponse:
    """HTMX polling endpoint for live progress bars."""
    conn = _conn()
    run = db.get_run(conn, run_id)
    if not run:
        raise HTTPException(404)
    return templates.TemplateResponse(
        request,
        "_progress.html",
        {"run": run, "progress": db.progress_for(conn, run_id)},
    )


@app.get("/run/{run_id}/job/{job_id}", response_class=HTMLResponse)
def job_view(request: Request, run_id: int, job_id: int) -> HTMLResponse:
    conn = _conn()
    row = conn.execute(
        "SELECT * FROM jobs WHERE id = ? AND run_id = ?", (job_id, run_id)
    ).fetchone()
    if not row:
        raise HTTPException(404)
    raw = json.dumps(json.loads(row["raw"]), indent=2) if row["raw"] else ""
    return templates.TemplateResponse(
        request, "job.html", {"row": row, "raw": raw, "run_id": run_id}
    )


# ---------------------------------------------------------------------------
# Logs: served plain-text for direct access, and as an HTML <pre> fragment
# for inline HTMX expansion in the diff tables.
# ---------------------------------------------------------------------------


def _log_root() -> Path | None:
    """Directory under which build logs are allowed to live.

    `minihydra serve` sets MINIHYDRA_LOG_ROOT to a known location. Without
    it, log access is disabled so a tampered or copied-around DB cannot be
    used to read arbitrary files.
    """
    raw = os.environ.get("MINIHYDRA_LOG_ROOT")
    if not raw:
        return None
    return Path(raw).resolve()


def _resolve_log(conn: sqlite3.Connection, run_id: int, job_id: int) -> Path:
    row = conn.execute(
        "SELECT build_log_path FROM jobs WHERE id = ? AND run_id = ?",
        (job_id, run_id),
    ).fetchone()
    if not row or not row["build_log_path"]:
        raise HTTPException(404, "no log")
    root = _log_root()
    if root is None:
        raise HTTPException(403, "log access disabled (MINIHYDRA_LOG_ROOT unset)")
    candidate = Path(row["build_log_path"]).resolve()
    try:
        candidate.relative_to(root)
    except ValueError as e:
        # Refuse anything outside the configured root; this is the
        # path-traversal guardrail.
        raise HTTPException(403, "log path outside allowed root") from e
    if not candidate.exists():
        raise HTTPException(404, "log file missing")
    return candidate


@app.get("/run/{run_id}/log/{job_id}", response_class=PlainTextResponse)
def job_log(run_id: int, job_id: int) -> PlainTextResponse:
    return PlainTextResponse(
        _resolve_log(_conn(), run_id, job_id).read_text(errors="replace")
    )


@app.get("/run/{run_id}/log/{job_id}/inline", response_class=HTMLResponse)
def job_log_inline(request: Request, run_id: int, job_id: int) -> HTMLResponse:
    """Lazy-loaded log fragment for inline HTMX expansion.

    Tail the log to a sane number of lines so a runaway build doesn't dump
    megabytes of output into the diff page.
    """
    text = _resolve_log(_conn(), run_id, job_id).read_text(errors="replace")
    tail_max = 400
    lines = text.splitlines()
    truncated = len(lines) > tail_max
    body = "\n".join(lines[-tail_max:])
    return templates.TemplateResponse(
        request,
        "_log.html",
        {
            "body": body,
            "truncated": truncated,
            "shown": min(tail_max, len(lines)),
            "total": len(lines),
            "run_id": run_id,
            "job_id": job_id,
        },
    )


# ---------------------------------------------------------------------------
# JSON API. The CLI consumes these via ASGI in-process; external agents/LLMs
# can hit the same endpoints over HTTP. All shapes are stable contracts.
# ---------------------------------------------------------------------------


def _row(row: Any) -> dict[str, Any]:
    return {k: row[k] for k in row.keys()}  # noqa: SIM118


@app.get("/api/runs")
def api_runs() -> JSONResponse:
    conn = _conn()
    return JSONResponse([_row(r) for r in db.runs(conn)])


@app.get("/api/run/{run_id}")
def api_run(run_id: int) -> JSONResponse:
    conn = _conn()
    run = db.get_run(conn, run_id)
    if not run:
        raise HTTPException(404)
    d = _get_diff(conn, run_id)
    base_count = sum(
        len(getattr(d, n))
        for n in ("removed", "newly_fixed", "changed", "unchanged", "still_broken")
    )
    head_count = sum(
        len(getattr(d, n))
        for n in ("added", "newly_broken", "changed", "unchanged", "still_broken")
    )
    return JSONResponse(
        {
            "run": _row(run),
            "summary": d.summary(),
            "counts": {"base": base_count, "head": head_count},
            "progress": [_row(p) for p in db.progress_for(conn, run_id)],
        }
    )


@app.get("/api/run/{run_id}/diff")
def api_diff(run_id: int, q: str = "") -> JSONResponse:
    conn = _conn()
    if not db.get_run(conn, run_id):
        raise HTTPException(404)
    d = _filter_diff(_get_diff(conn, run_id), q)
    return JSONResponse({"summary": d.summary(), **_serialize_diff(d)})


@app.get("/api/run/{run_id}/section/{name}")
def api_section(
    run_id: int,
    name: str,
    q: str = "",
    offset: int = 0,
    limit: int | None = None,
) -> JSONResponse:
    if name not in SECTIONS:
        raise HTTPException(404, f"unknown section {name}")
    conn = _conn()
    if not db.get_run(conn, run_id):
        raise HTTPException(404)
    d = _filter_diff(_get_diff(conn, run_id), q)
    entries = getattr(d, name)
    total = len(entries)
    sliced = entries[offset : (offset + limit) if limit is not None else None]
    return JSONResponse(
        {
            "section": name,
            "total": total,
            "offset": offset,
            "limit": limit,
            "entries": [asdict(e) for e in sliced],
        }
    )


@app.get("/api/run/{run_id}/job/{job_id}")
def api_job(run_id: int, job_id: int) -> JSONResponse:
    conn = _conn()
    row = conn.execute(
        "SELECT * FROM jobs WHERE id = ? AND run_id = ?", (job_id, run_id)
    ).fetchone()
    if not row:
        raise HTTPException(404)
    out = _row(row)
    if out.get("raw"):
        out["raw_parsed"] = json.loads(out["raw"])
    return JSONResponse(out)


@app.get("/api/run/{run_id}/progress")
def api_progress(run_id: int) -> JSONResponse:
    conn = _conn()
    if not db.get_run(conn, run_id):
        raise HTTPException(404)
    return JSONResponse([_row(p) for p in db.progress_for(conn, run_id)])


def _serialize_diff(d: diff.Diff) -> dict[str, list[dict[str, Any]]]:
    return {name: [asdict(e) for e in getattr(d, name)] for name in SECTIONS}
