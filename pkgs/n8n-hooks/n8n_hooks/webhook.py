"""Shared helper to POST JSON to an n8n webhook."""

from __future__ import annotations

import json
import sys
import urllib.request
from typing import Any

from n8n_hooks.config import HookConfig


def post(hook: HookConfig, payload: dict[str, Any]) -> dict[str, Any]:
    """POST *payload* as JSON to the webhook, return parsed response."""
    data = json.dumps(payload).encode()

    headers: dict[str, str] = {"Content-Type": "application/json"}
    if hook.token:
        headers["Authorization"] = f"Bearer {hook.token}"

    req = urllib.request.Request(hook.url, data=data, headers=headers, method="POST")

    try:
        with urllib.request.urlopen(req) as resp:
            body = resp.read().decode()
            try:
                return json.loads(body)  # type: ignore[no-any-return]
            except json.JSONDecodeError:
                return {"status": resp.status, "body": body}
    except urllib.error.HTTPError as exc:
        error_body = exc.read().decode() if exc.fp else ""
        print(
            f"n8n-hooks: HTTP {exc.code} from {hook.url}: {error_body}",
            file=sys.stderr,
        )
        sys.exit(1)
    except urllib.error.URLError as exc:
        print(f"n8n-hooks: connection error: {exc.reason}", file=sys.stderr)
        sys.exit(1)
