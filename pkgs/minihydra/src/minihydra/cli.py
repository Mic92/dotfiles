"""Command-line entrypoint."""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Any

import httpx
from rich.console import Console
from rich.progress import (
    BarColumn,
    MofNCompleteColumn,
    Progress,
    SpinnerColumn,
    TextColumn,
    TimeElapsedColumn,
)
from rich.table import Table

from minihydra import build as build_mod
from minihydra import db, diff
from minihydra import eval as eval_mod


def default_db_path() -> Path:
    base = os.environ.get("XDG_DATA_HOME") or str(Path.home() / ".local/share")
    return Path(base) / "minihydra" / "minihydra.db"


def default_log_root() -> Path:
    base = os.environ.get("XDG_CACHE_HOME") or str(Path.home() / ".cache")
    return Path(base) / "minihydra" / "logs"


def make_progress() -> Progress:
    return Progress(
        SpinnerColumn(),
        TextColumn("[bold blue]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeElapsedColumn(),
    )


# ---------------------------------------------------------------------------
# API client. Read-only subcommands wrap the FastAPI app in-process via ASGI,
# so `minihydra list/show/diff/summary/job` and `minihydra serve` share data
# shapes. No network is opened, so no auth concerns for the CLI path.
# ---------------------------------------------------------------------------


def _api_client(db_path: Path) -> httpx.Client:
    if not db_path.exists():
        msg = f"db not found: {db_path}"
        raise SystemExit(msg)
    os.environ["MINIHYDRA_DB"] = str(db_path)
    from minihydra.web import app

    return httpx.Client(
        transport=httpx.ASGITransport(app=app),
        base_url="http://minihydra",
    )


def _get(client: httpx.Client, path: str) -> Any:
    r = client.get(path)
    if r.status_code == 404:
        msg = r.json().get("detail", "not found")
        print(msg, file=sys.stderr)
        raise SystemExit(1)
    r.raise_for_status()
    return r.json()


def _eval_side(
    *,
    conn: db.sqlite3.Connection,
    prog: Progress,
    run_id: int,
    repo: Path,
    side: str,
    rev: str,
    attr: str,
    flake_override: str | None,
    workers: int,
) -> None:
    """Evaluate one side and stream rows into the run.

    Extracted so ``cmd_run`` and ``cmd_rerun`` can share the same eval loop;
    rerun calls this only for the head side, with the base copied from a
    prior run.
    """
    task = prog.add_task(f"eval {side} ({rev[:8]})", total=None)
    db.update_progress(
        conn,
        run_id,
        f"eval-{side}",
        current=0,
        total=0,
        message=f"starting {rev[:8]}",
    )
    with eval_mod.worktree_at(repo, rev) as wt:
        flake_ref = flake_override or str(wt)

        def _cb(n: int, t: object = task, s: str = side) -> None:
            prog.update(t, completed=n, total=max(n, n + 1))
            db.update_progress(
                conn,
                run_id,
                f"eval-{s}",
                current=n,
                total=n,
                message=f"evaluated {n} attrs",
            )

        eval_mod.evaluate_side(
            conn,
            run_id=run_id,
            side=side,
            flake_ref=flake_ref,
            attr=attr,
            workers=workers,
            on_progress=_cb,
        )
    prog.update(task, total=prog.tasks[task].completed)


def _maybe_build_head(
    *,
    conn: db.sqlite3.Connection,
    console: Console,
    run_id: int,
    log_root: Path,
    want_build: bool,
) -> None:
    if not want_build:
        return
    if not build_mod.have_nix_store():
        console.print("[red]nix-store not in PATH; skipping build[/]")
        return
    with make_progress() as prog:
        head_rows = [
            r
            for r in db.jobs_for(conn, run_id, side="head")
            if r["drv_path"] and not r["error"]
        ]
        task = prog.add_task("build head", total=len(head_rows))

        def cb(i: int, total: int, attr: str) -> None:
            prog.update(task, completed=i, total=total, description=f"build {attr}")
            db.update_progress(
                conn,
                run_id,
                "build",
                current=i,
                total=total,
                message=attr,
            )

        build_mod.build_jobs(
            conn,
            run_id,
            side="head",
            log_dir=log_root / "build",
            on_progress=cb,
        )


def cmd_run(args: argparse.Namespace) -> int:
    console = Console()
    repo_dir = Path(args.repo or ".").resolve()
    repo = eval_mod.find_repo_root(repo_dir)

    head_rev = args.head or "HEAD"
    base_rev = args.base or "HEAD~1"
    head_resolved = eval_mod.resolve_rev(repo, head_rev)
    base_resolved = eval_mod.resolve_rev(repo, base_rev)

    if base_resolved == head_resolved:
        console.print(
            "[yellow]base and head resolve to same commit; nothing to diff[/]"
        )
        return 1

    db_path = Path(args.db) if args.db else default_db_path()
    conn = db.connect(db_path)
    run_id = db.create_run(
        conn,
        flake_ref=args.flake or str(repo),
        attr=args.attr,
        base_rev=base_rev,
        head_rev=head_rev,
        base_resolved=base_resolved,
        head_resolved=head_resolved,
    )
    console.print(
        f"[bold]run #{run_id}[/] db=[cyan]{db_path}[/] "
        f"base=[magenta]{base_resolved[:12]}[/] head=[green]{head_resolved[:12]}[/] "
        f"attr=[bold]{args.attr}[/]"
    )

    log_root = default_log_root() / f"run-{run_id}"

    try:
        with make_progress() as prog:
            for side, rev in (("base", base_resolved), ("head", head_resolved)):
                _eval_side(
                    conn=conn,
                    prog=prog,
                    run_id=run_id,
                    repo=repo,
                    side=side,
                    rev=rev,
                    attr=args.attr,
                    workers=args.workers,
                )

        _maybe_build_head(
            conn=conn,
            console=console,
            run_id=run_id,
            log_root=log_root,
            want_build=args.build,
        )
        db.set_run_status(conn, run_id, "done")
    except Exception as e:
        db.set_run_status(conn, run_id, f"error: {e}")
        raise

    base_rows = db.jobs_for(conn, run_id, side="base")
    head_rows = db.jobs_for(conn, run_id, side="head")
    d = diff.compute(base_rows, head_rows)
    summary = d.summary()
    if args.json:
        print(
            json.dumps(
                {
                    "run_id": run_id,
                    "db": str(db_path),
                    "base": base_resolved,
                    "head": head_resolved,
                    "attr": args.attr,
                    "summary": summary,
                },
                indent=2,
            )
        )
    else:
        console.print()
        console.print("[bold]diff summary[/]")
        for k, v in summary.items():
            console.print(f"  {k:14s} {v}")
        console.print(
            f"\nweb UI: [cyan]minihydra serve --db {db_path}[/] "
            f"then http://127.0.0.1:8765/run/{run_id}"
        )
    return 0


def cmd_rerun(args: argparse.Namespace) -> int:
    """Re-evaluate head against an existing run's base, skipping base eval.

    The base eval on a real nixpkgs takes minutes; if you're iterating on a
    fix while ``base`` doesn't move (e.g. it tracks ``origin/main``), there's
    no reason to re-run it. ``rerun`` copies the source run's base rows and
    only evaluates the new head.
    """
    console = Console()
    db_path = Path(args.db) if args.db else default_db_path()
    conn = db.connect(db_path)

    src = db.get_run(conn, args.run_id) if args.run_id else db.latest_run(conn)
    if src is None:
        print(
            "no prior run to rerun. use `minihydra run` first.",
            file=sys.stderr,
        )
        return 1

    repo_dir = Path(args.repo or ".").resolve()
    repo = eval_mod.find_repo_root(repo_dir)
    head_rev = args.head or "HEAD"
    head_resolved = eval_mod.resolve_rev(repo, head_rev)

    if head_resolved == src["head_resolved"] and not args.force:
        console.print(
            f"[yellow]head {head_resolved[:12]} matches run #{src['id']}'s head; "
            f"nothing to re-evaluate. pass --force to do it anyway.[/]"
        )
        return 1
    if head_resolved == src["base_resolved"]:
        console.print("[yellow]new head equals base; nothing to diff[/]")
        return 1

    run_id = db.create_run(
        conn,
        flake_ref=args.flake or src["flake_ref"],
        attr=src["attr"],
        base_rev=src["base_rev"],
        head_rev=head_rev,
        base_resolved=src["base_resolved"],
        head_resolved=head_resolved,
    )

    with db.transaction(conn):
        copied = db.copy_side_jobs(conn, src["id"], run_id, "base")
    db.update_progress(
        conn,
        run_id,
        "eval-base",
        current=copied,
        total=copied,
        message=f"reused base from run #{src['id']}",
    )
    console.print(
        f"[bold]run #{run_id}[/] db=[cyan]{db_path}[/] "
        f"base=[magenta]{src['base_resolved'][:12]}[/] (reused from #{src['id']}, "
        f"{copied} rows) head=[green]{head_resolved[:12]}[/] "
        f"attr=[bold]{src['attr']}[/]"
    )

    log_root = default_log_root() / f"run-{run_id}"
    try:
        with make_progress() as prog:
            _eval_side(
                conn=conn,
                prog=prog,
                run_id=run_id,
                repo=repo,
                side="head",
                rev=head_resolved,
                attr=src["attr"],
                flake_override=args.flake,
                workers=args.workers,
            )
        _maybe_build_head(
            conn=conn,
            console=console,
            run_id=run_id,
            log_root=log_root,
            want_build=args.build,
        )
        db.set_run_status(conn, run_id, "done")
    except Exception as e:
        db.set_run_status(conn, run_id, f"error: {e}")
        raise

    base_rows = db.jobs_for(conn, run_id, side="base")
    head_rows = db.jobs_for(conn, run_id, side="head")
    d = diff.compute(base_rows, head_rows)
    summary = d.summary()
    if args.json:
        print(
            json.dumps(
                {
                    "run_id": run_id,
                    "src_run_id": src["id"],
                    "db": str(db_path),
                    "base": src["base_resolved"],
                    "head": head_resolved,
                    "attr": src["attr"],
                    "summary": summary,
                },
                indent=2,
            )
        )
    else:
        console.print()
        console.print("[bold]diff summary[/]")
        for k, v in summary.items():
            console.print(f"  {k:14s} {v}")
        console.print(
            f"\nweb UI: [cyan]minihydra serve --db {db_path}[/] "
            f"then http://127.0.0.1:8765/run/{run_id}"
        )
    return 0


_LOOPBACK_HOSTS = {"127.0.0.1", "::1", "localhost"}


def cmd_serve(args: argparse.Namespace) -> int:
    import uvicorn

    db_path = Path(args.db) if args.db else default_db_path()
    if not db_path.exists():
        print(f"db not found: {db_path}", file=sys.stderr)
        return 1
    if args.host not in _LOOPBACK_HOSTS and not args.allow_public:
        print(
            f"refusing to bind {args.host}: web UI has no auth. "
            "pass --allow-public if you really mean it.",
            file=sys.stderr,
        )
        return 2
    os.environ["MINIHYDRA_DB"] = str(db_path)
    # Confine log-file reads to the default log root. Without this, a poisoned
    # build_log_path in a copied-around DB could exfiltrate arbitrary files
    # readable by the server user.
    os.environ.setdefault("MINIHYDRA_LOG_ROOT", str(default_log_root()))
    uvicorn.run(
        "minihydra.web:app",
        host=args.host,
        port=args.port,
        log_level="info",
        reload=False,
    )
    return 0


def cmd_list(args: argparse.Namespace) -> int:
    console = Console()
    db_path = Path(args.db) if args.db else default_db_path()
    with _api_client(db_path) as c:
        runs = _get(c, "/api/runs")
    if args.json:
        print(json.dumps(runs, indent=2, default=str))
        return 0
    t = Table(title="runs")
    for col in ("id", "attr", "base", "head", "status", "created"):
        t.add_column(col)
    for r in runs:
        t.add_row(
            str(r["id"]),
            r["attr"],
            (r["base_resolved"] or "")[:12],
            (r["head_resolved"] or "")[:12],
            r["status"],
            f"{r['created_at']:.0f}",
        )
    console.print(t)
    return 0


def cmd_show(args: argparse.Namespace) -> int:
    console = Console()
    db_path = Path(args.db) if args.db else default_db_path()
    with _api_client(db_path) as c:
        payload = _get(c, f"/api/run/{args.run_id}")
    if args.json:
        print(json.dumps(payload, indent=2, default=str))
        return 0
    run = payload["run"]
    console.print(
        f"[bold]run #{run['id']}[/] [{run['status']}] attr=[cyan]{run['attr']}[/]"
    )
    console.print(
        f"  base {run['base_rev']} ({run['base_resolved'][:12]})\n"
        f"  head {run['head_rev']} ({run['head_resolved'][:12]})"
    )
    console.print("[bold]summary[/]")
    for k, v in payload["summary"].items():
        console.print(f"  {k:14s} {v}")
    return 0


def cmd_diff(args: argparse.Namespace) -> int:
    console = Console()
    db_path = Path(args.db) if args.db else default_db_path()
    with _api_client(db_path) as c:
        if args.section:
            payload = _get(c, f"/api/run/{args.run_id}/section/{args.section}")
        else:
            payload = _get(c, f"/api/run/{args.run_id}/diff")
    if args.json:
        print(json.dumps(payload, indent=2, default=str))
        return 0
    if args.section:
        for e in payload["entries"]:
            console.print(f"  {e['attr']}")
    else:
        for name, entries in payload.items():
            if name == "summary" or not entries:
                continue
            console.print(f"[bold]{name}[/] ({len(entries)})")
            for e in entries[: args.limit]:
                console.print(f"  {e['attr']}")
            if len(entries) > args.limit:
                console.print(f"  ... {len(entries) - args.limit} more")
    return 0


def cmd_summary(args: argparse.Namespace) -> int:
    """Compact text summary, suitable to paste into an LLM context window."""
    db_path = Path(args.db) if args.db else default_db_path()
    with _api_client(db_path) as c:
        meta = _get(c, f"/api/run/{args.run_id}")
        diff_payload = _get(c, f"/api/run/{args.run_id}/diff")
    run = meta["run"]
    lines = [
        f"minihydra run #{run['id']} attr={run['attr']}",
        (
            f"base={run['base_resolved'][:12]} "
            f"head={run['head_resolved'][:12]} status={run['status']}"
        ),
        "summary: " + ", ".join(f"{k}={v}" for k, v in meta["summary"].items()),
    ]
    for name in ("newly_broken", "newly_fixed", "added", "removed", "changed"):
        entries = diff_payload.get(name, [])
        if not entries:
            continue
        attrs = [e["attr"] for e in entries[: args.limit]]
        more = (
            f" (+{len(entries) - args.limit} more)" if len(entries) > args.limit else ""
        )
        lines.append(f"{name}: {', '.join(attrs)}{more}")
        if name in ("newly_broken", "still_broken"):
            for e in entries[: args.limit]:
                err = (e.get("head") or {}).get("error") or ""
                if err:
                    first = err.splitlines()[0][:200]
                    lines.append(f"  {e['attr']}: {first}")
    print("\n".join(lines))
    return 0


def cmd_job(args: argparse.Namespace) -> int:
    db_path = Path(args.db) if args.db else default_db_path()
    with _api_client(db_path) as c:
        payload = _get(c, f"/api/run/{args.run_id}/job/{args.job_id}")
    print(json.dumps(payload, indent=2, default=str))
    return 0


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="minihydra")
    sub = p.add_subparsers(dest="cmd", required=True)

    r = sub.add_parser("run", help="evaluate base + head and store diff")
    r.add_argument("--repo", help="git repo path (default: cwd)")
    r.add_argument("--flake", help="flake ref override (default: worktree path)")
    r.add_argument(
        "--attr",
        required=True,
        help="attribute path to evaluate, e.g. legacyPackages.x86_64-linux",
    )
    r.add_argument("--base", help="baseline revision (default HEAD~1)")
    r.add_argument("--head", help="head revision (default HEAD)")
    r.add_argument("--workers", type=int, default=4)
    r.add_argument("--build", action="store_true", help="build head drvs after eval")
    r.add_argument("--db", help="sqlite db path")
    r.add_argument("--json", action="store_true", help="emit final summary as JSON")
    r.set_defaults(func=cmd_run)

    rr = sub.add_parser(
        "rerun",
        help="re-evaluate head against a prior run's base (skips base eval)",
    )
    rr.add_argument(
        "run_id",
        nargs="?",
        type=int,
        help="source run id (default: most recent run in the db)",
    )
    rr.add_argument("--repo", help="git repo path (default: cwd)")
    rr.add_argument("--flake", help="flake ref override")
    rr.add_argument("--head", help="head revision (default: HEAD)")
    rr.add_argument("--workers", type=int, default=4)
    rr.add_argument("--build", action="store_true")
    rr.add_argument("--db", help="sqlite db path")
    rr.add_argument("--json", action="store_true")
    rr.add_argument(
        "--force",
        action="store_true",
        help="re-evaluate even if head matches the source run's head",
    )
    rr.set_defaults(func=cmd_rerun)

    s = sub.add_parser("serve", help="run web UI (loopback by default)")
    s.add_argument("--db", help="sqlite db path")
    s.add_argument("--host", default="127.0.0.1")
    s.add_argument("--port", type=int, default=8765)
    s.add_argument(
        "--allow-public",
        action="store_true",
        help=(
            "required to bind a non-loopback address. The web UI has no "
            "authentication and exposes eval errors and build log paths."
        ),
    )
    s.set_defaults(func=cmd_serve)

    ls = sub.add_parser("list", help="list runs (use --json for LLM/agents)")
    ls.add_argument("--db")
    ls.add_argument("--json", action="store_true")
    ls.set_defaults(func=cmd_list)

    sh = sub.add_parser("show", help="show run metadata + summary")
    sh.add_argument("run_id", type=int)
    sh.add_argument("--db")
    sh.add_argument("--json", action="store_true")
    sh.set_defaults(func=cmd_show)

    df = sub.add_parser("diff", help="diff sections (--json for full data)")
    df.add_argument("run_id", type=int)
    df.add_argument(
        "--section",
        choices=[
            "added",
            "removed",
            "changed",
            "unchanged",
            "newly_broken",
            "newly_fixed",
            "still_broken",
        ],
    )
    df.add_argument("--limit", type=int, default=20)
    df.add_argument("--db")
    df.add_argument("--json", action="store_true")
    df.set_defaults(func=cmd_diff)

    sm = sub.add_parser("summary", help="compact text summary for LLM context")
    sm.add_argument("run_id", type=int)
    sm.add_argument("--limit", type=int, default=10)
    sm.add_argument("--db")
    sm.set_defaults(func=cmd_summary)

    jb = sub.add_parser("job", help="dump full json for a single job")
    jb.add_argument("run_id", type=int)
    jb.add_argument("job_id", type=int)
    jb.add_argument("--db")
    jb.set_defaults(func=cmd_job)

    return p


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
