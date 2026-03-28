"""github hook — read-only proxy for the GitHub REST API via n8n.

The n8n workflow holds the GitHub token and *forces GET server-side*, so
this CLI can never mutate anything even if abused.  For write
operations, use `gh` directly.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import urllib.request
from pathlib import Path
from typing import Any

from n8n_hooks.config import HookConfig
from n8n_hooks.webhook import post

HOOK_NAME = "github"
SPEC_URL = (
    "https://raw.githubusercontent.com/github/rest-api-description/"
    "main/descriptions/api.github.com/api.github.com.json"
)


def register(subparsers: argparse._SubParsersAction[argparse.ArgumentParser]) -> None:
    p = subparsers.add_parser(
        "github",
        help="Call the GitHub REST API via n8n (read-only)",
        description="Proxy a GET request to api.github.com.",
    )
    p.add_argument(
        "path",
        help="Path under api.github.com, e.g. /repos/Mic92/dotfiles/issues. "
        "Use 'discover <term>' to grep the API spec.",
    )
    p.add_argument(
        "term",
        nargs="?",
        default="",
        help="Search term for discover mode",
    )
    p.add_argument(
        "-q",
        "--query",
        action="append",
        default=[],
        metavar="K=V",
        help="Query parameter (repeatable)",
    )
    p.set_defaults(func=run)


def build_payload(args: argparse.Namespace) -> dict[str, object]:
    payload: dict[str, object] = {"path": args.path}

    query: dict[str, str] = {}
    for kv in args.query:
        k, _, v = kv.partition("=")
        query[k] = v
    if query:
        payload["query"] = query

    return payload


def _cache_path() -> Path:
    cache = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache"))
    return cache / "n8n-hooks" / "github-api.json"


def _load_spec() -> dict[str, Any]:
    p = _cache_path()
    if not p.exists():
        p.parent.mkdir(parents=True, exist_ok=True)
        print(f"n8n-hooks: fetching {SPEC_URL} (~12MB, cached)", file=sys.stderr)
        with urllib.request.urlopen(SPEC_URL) as r, open(p, "wb") as f:
            f.write(r.read())
    with open(p) as f:
        return json.load(f)  # type: ignore[no-any-return]


def _resolve_ref(spec: dict[str, Any], ref: str) -> dict[str, Any]:
    node: Any = spec
    for part in ref.lstrip("#/").split("/"):
        node = node.get(part, {})
    return node if isinstance(node, dict) else {}


def _fmt_schema(spec: dict[str, Any], schema: dict[str, Any], indent: str) -> None:
    if "$ref" in schema:
        schema = _resolve_ref(spec, schema["$ref"])
    req = set(schema.get("required", []))
    for name, prop in schema.get("properties", {}).items():
        if "$ref" in prop:
            prop = _resolve_ref(spec, prop["$ref"])
        t = prop.get("type", "object")
        if t == "array":
            item = prop.get("items", {})
            if "$ref" in item:
                item = _resolve_ref(spec, item["$ref"])
            t = f"[{item.get('type', 'object')}]"
        mark = "*" if name in req else " "
        desc = str(prop.get("description", "")).split("\n")[0]
        print(f"{indent}{mark} {name}: {t}  {desc}".rstrip())


def _show_path(spec: dict[str, Any], path: str) -> None:
    for method, op in sorted(spec["paths"][path].items()):
        if not isinstance(op, dict):
            continue
        ro = "" if method == "get" else "  (write — not available via this hook)"
        print(f"\n{method.upper()} {path}{ro}")
        if s := op.get("summary"):
            print(f"  {s}")
        for p in op.get("parameters", []):
            if "$ref" in p:
                p = _resolve_ref(spec, p["$ref"])
            mark = "*" if p.get("required") else " "
            t = p.get("schema", {}).get("type", "")
            desc = str(p.get("description", "")).split("\n")[0]
            print(f"  {mark} {p.get('name')} ({p.get('in')}:{t})  {desc}".rstrip())
        body = op.get("requestBody", {}).get("content", {})
        if schema := body.get("application/json", {}).get("schema"):
            print("  body:")
            _fmt_schema(spec, schema, "    ")


def _discover(pattern: str) -> None:
    """Grep endpoints, or show params if pattern is an exact path."""
    spec = _load_spec()
    paths = spec.get("paths", {})

    if pattern in paths:
        _show_path(spec, pattern)
        return

    pat = pattern.lower()
    for path, ops in sorted(paths.items()):
        methods = ",".join(sorted(m for m in ops if isinstance(ops[m], dict)))
        summaries = " | ".join(
            o.get("summary", "") for o in ops.values() if isinstance(o, dict)
        )
        line = f"{path} [{methods}]  {summaries}"
        if not pat or pat in line.lower():
            print(line)


def run(args: argparse.Namespace, config: dict[str, HookConfig]) -> None:
    if args.path == "discover":
        _discover(args.term)
        return

    if HOOK_NAME not in config:
        print(f"n8n-hooks: no '{HOOK_NAME}' section in config", file=sys.stderr)
        sys.exit(1)

    result = post(config[HOOK_NAME], build_payload(args))

    status = result.get("status")
    body = result.get("body", result)
    print(json.dumps(body, indent=2))
    if isinstance(status, int) and status >= 400:
        sys.exit(1)
