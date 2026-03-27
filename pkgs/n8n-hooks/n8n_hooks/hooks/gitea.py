"""gitea hook — thin proxy for the Gitea REST API via n8n.

The n8n workflow holds the Gitea token; this CLI just forwards
method/path/body/query to the webhook.  No convenience wrappers — the
skill file documents common endpoints instead.
"""

from __future__ import annotations

import argparse
import json
import sys
import urllib.request
from typing import Any

from n8n_hooks.config import HookConfig
from n8n_hooks.webhook import post

HOOK_NAME = "gitea"
SWAGGER_URL = "https://git.thalheim.io/swagger.v1.json"


def register(subparsers: argparse._SubParsersAction[argparse.ArgumentParser]) -> None:
    p = subparsers.add_parser(
        "gitea",
        help="Call the Gitea REST API via n8n",
        description="Proxy a request to git.thalheim.io/api/v1.",
    )
    p.add_argument(
        "method",
        choices=["GET", "POST", "PUT", "PATCH", "DELETE", "discover"],
        help="HTTP method, or 'discover' to grep the API spec",
    )
    p.add_argument("path", help="Path under /api/v1, e.g. /user/repos")
    p.add_argument(
        "-d", "--data", help="JSON request body (string, @file, or - for stdin)"
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


def _read_data(value: str | None) -> Any:
    if value is None:
        return None
    if value == "-":
        value = sys.stdin.read()
    elif value.startswith("@"):
        with open(value[1:]) as f:
            value = f.read()
    return json.loads(value)


def build_payload(args: argparse.Namespace) -> dict[str, Any]:
    payload: dict[str, Any] = {"method": args.method, "path": args.path}

    query: dict[str, str] = {}
    for kv in args.query:
        k, _, v = kv.partition("=")
        query[k] = v
    if query:
        payload["query"] = query

    body = _read_data(args.data)
    if body is not None:
        payload["body"] = body

    return payload


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
        desc = prop.get("description", "")
        print(f"{indent}{mark} {name}: {t}  {desc}".rstrip())


def _show_path(spec: dict[str, Any], path: str) -> None:
    ops = spec["paths"][path]
    for method, op in sorted(ops.items()):
        if not isinstance(op, dict):
            continue
        print(f"\n{method.upper()} {path}")
        if s := op.get("summary"):
            print(f"  {s}")
        for p in op.get("parameters", []):
            if "$ref" in p:
                p = _resolve_ref(spec, p["$ref"])
            loc = p.get("in", "")
            if loc == "body":
                print("  body:")
                _fmt_schema(spec, p.get("schema", {}), "    ")
            else:
                mark = "*" if p.get("required") else " "
                t = p.get("type", "")
                desc = p.get("description", "")
                print(f"  {mark} {p.get('name')} ({loc}:{t})  {desc}".rstrip())


def _discover(pattern: str) -> None:
    """Grep endpoints, or show params if pattern is an exact path."""
    with urllib.request.urlopen(SWAGGER_URL) as resp:
        spec = json.load(resp)
    paths = spec.get("paths", {})

    if pattern in paths:
        _show_path(spec, pattern)
        return

    pat = pattern.lower()
    for path, ops in sorted(paths.items()):
        methods = ",".join(sorted(ops))
        summaries = " | ".join(
            o.get("summary", "") for o in ops.values() if isinstance(o, dict)
        )
        line = f"{path} [{methods}]  {summaries}"
        if not pat or pat in line.lower():
            print(line)


def run(args: argparse.Namespace, config: dict[str, HookConfig]) -> None:
    if args.method == "discover":
        _discover(args.path)
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
