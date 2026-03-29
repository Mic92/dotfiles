"""slack hook — read-only Slack context via n8n.

The n8n workflow holds the Slack token.  This CLI forwards one of a
fixed set of operations to the webhook so the agent can search
messages, list channels/users, and read channel history / thread
replies without holding credentials.
"""

from __future__ import annotations

import argparse
import json
import sys

from n8n_hooks.config import HookConfig
from n8n_hooks.webhook import post

HOOK_NAME = "slack"


def register(subparsers: argparse._SubParsersAction[argparse.ArgumentParser]) -> None:
    p = subparsers.add_parser(
        "slack",
        help="Query Slack via n8n (read-only)",
        description="Search / list / read Slack messages, channels and users.",
    )
    sub = p.add_subparsers(dest="op", required=True)

    s = sub.add_parser("search", help="Search messages")
    s.add_argument("query", help="Slack search query, e.g. 'in:#dev foo'")
    s.add_argument("-n", "--limit", type=int, default=20)

    h = sub.add_parser("history", help="Channel history")
    h.add_argument("channel", help="Channel ID, e.g. C0123456789")
    h.add_argument("-n", "--limit", type=int, default=50)

    r = sub.add_parser("replies", help="Thread replies")
    r.add_argument("channel", help="Channel ID")
    r.add_argument("thread_ts", help="Parent message ts, e.g. 1712345678.123456")
    r.add_argument("-n", "--limit", type=int, default=50)

    sub.add_parser("list-channels", help="List all non-archived channels")
    sub.add_parser("list-users", help="List all users")

    p.set_defaults(func=run)


def build_payload(args: argparse.Namespace) -> dict[str, object]:
    payload: dict[str, object] = {"operation": args.op}
    for k in ("query", "channel", "thread_ts", "limit"):
        v = getattr(args, k, None)
        if v is not None:
            payload[k] = v
    return payload


def run(args: argparse.Namespace, config: dict[str, HookConfig]) -> None:
    if HOOK_NAME not in config:
        print(f"n8n-hooks: no '{HOOK_NAME}' section in config", file=sys.stderr)
        sys.exit(1)

    result = post(config[HOOK_NAME], build_payload(args))
    print(json.dumps(result, indent=2))
