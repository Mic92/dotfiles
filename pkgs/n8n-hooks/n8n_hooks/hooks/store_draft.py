"""store-draft hook — create an email draft via n8n webhook.

Reads the spec at /tmp/spec: the webhook accepts JSON with to, subject,
body_plain, body_html (required) plus optional from, cc, bcc,
in_reply_to, references, and attachments (base64-encoded).
"""

from __future__ import annotations

import argparse
import base64
import json
import mimetypes
import sys
from pathlib import Path
from typing import Any

from n8n_hooks.config import HookConfig
from n8n_hooks.webhook import post

HOOK_NAME = "store-draft"


def register(subparsers: argparse._SubParsersAction[argparse.ArgumentParser]) -> None:
    p = subparsers.add_parser(
        "store-draft",
        help="Store an email draft in IMAP via n8n",
    )
    p.add_argument("--to", required=True, help="Recipient address")
    p.add_argument("--subject", required=True, help="Email subject")
    p.add_argument(
        "--body-plain", required=True, help="Plain-text body (or - for stdin)"
    )
    p.add_argument(
        "--body-html",
        default=None,
        help="HTML body (default: wrap plain text in <pre>)",
    )
    p.add_argument(
        "--from",
        dest="from_addr",
        default=None,
        help="Sender (default: server-side default)",
    )
    p.add_argument("--cc", default=None, help="CC addresses")
    p.add_argument("--bcc", default=None, help="BCC addresses")
    p.add_argument("--in-reply-to", default=None, help="Message-ID being replied to")
    p.add_argument("--references", default=None, help="References header value")
    p.add_argument(
        "--attach",
        action="append",
        default=[],
        metavar="FILE",
        help="Attach a file (repeatable)",
    )
    p.set_defaults(func=run)


def _read_body(value: str) -> str:
    if value == "-":
        return sys.stdin.read()
    return value


def _encode_attachment(path_str: str) -> dict[str, str]:
    path = Path(path_str)
    if not path.is_file():
        print(f"n8n-hooks: attachment not found: {path}", file=sys.stderr)
        sys.exit(1)
    mime, _ = mimetypes.guess_type(str(path))
    if mime is None:
        mime = "application/octet-stream"
    content = base64.b64encode(path.read_bytes()).decode("ascii")
    return {
        "filename": path.name,
        "mimetype": mime,
        "content": content,
    }


def build_payload(args: argparse.Namespace) -> dict[str, Any]:
    """Build the JSON payload from parsed CLI args."""
    body_plain = _read_body(args.body_plain)

    payload: dict[str, Any] = {
        "to": args.to,
        "subject": args.subject,
        "body_plain": body_plain,
        "body_html": args.body_html if args.body_html else f"<pre>{body_plain}</pre>",
    }

    for key, attr in [
        ("from", "from_addr"),
        ("cc", "cc"),
        ("bcc", "bcc"),
        ("in_reply_to", "in_reply_to"),
        ("references", "references"),
    ]:
        val = getattr(args, attr)
        if val is not None:
            payload[key] = val

    if args.attach:
        payload["attachments"] = [_encode_attachment(a) for a in args.attach]

    return payload


def run(args: argparse.Namespace, config: dict[str, HookConfig]) -> None:
    if HOOK_NAME not in config:
        print(
            f"n8n-hooks: no '{HOOK_NAME}' section in config",
            file=sys.stderr,
        )
        sys.exit(1)

    payload = build_payload(args)
    result = post(config[HOOK_NAME], payload)
    print(json.dumps(result, indent=2))
