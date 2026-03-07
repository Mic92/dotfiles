"""Tests for the store-draft hook."""

from __future__ import annotations

import argparse
import base64
import json
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from threading import Thread
from typing import Any
from unittest.mock import patch

import pytest

from n8n_hooks.config import HookConfig
from n8n_hooks.hooks.store_draft import build_payload, run


def _make_args(**overrides: Any) -> argparse.Namespace:
    defaults: dict[str, Any] = {
        "to": "alice@example.com",
        "subject": "Test subject",
        "body_plain": "Hello, world!",
        "body_html": None,
        "from_addr": None,
        "cc": None,
        "bcc": None,
        "in_reply_to": None,
        "references": None,
        "attach": [],
    }
    defaults.update(overrides)
    return argparse.Namespace(**defaults)


def test_attachment(tmp_path: Path) -> None:
    pdf = tmp_path / "report.pdf"
    pdf.write_bytes(b"%PDF-fake")

    args = _make_args(attach=[str(pdf)])
    payload = build_payload(args)

    assert len(payload["attachments"]) == 1
    att = payload["attachments"][0]
    assert att["filename"] == "report.pdf"
    assert att["mimetype"] == "application/pdf"
    assert base64.b64decode(att["content"]) == b"%PDF-fake"


def test_missing_attachment() -> None:
    args = _make_args(attach=["/nonexistent/file.txt"])
    with pytest.raises(SystemExit):
        build_payload(args)


def test_stdin_body() -> None:
    args = _make_args(body_plain="-")
    with patch("sys.stdin") as mock_stdin:
        mock_stdin.read.return_value = "from stdin"
        payload = build_payload(args)
    assert payload["body_plain"] == "from stdin"


# ---------- end-to-end with a fake server ----------


class _FakeWebhookHandler(BaseHTTPRequestHandler):
    """Captures the last POST body and replies 200."""

    last_payload: dict[str, Any] = {}

    def do_POST(self) -> None:
        length = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(length)
        _FakeWebhookHandler.last_payload = json.loads(body)

        auth = self.headers.get("Authorization", "")
        if auth != "Bearer test-secret":
            self.send_response(403)
            self.end_headers()
            self.wfile.write(b'{"error":"unauthorized"}')
            return

        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(b'{"status":"ok"}')

    def log_message(self, *_args: Any) -> None:
        pass


def test_end_to_end(capsys: pytest.CaptureFixture[str]) -> None:
    server = HTTPServer(("127.0.0.1", 0), _FakeWebhookHandler)
    port = server.server_address[1]
    thread = Thread(target=server.handle_request, daemon=True)
    thread.start()

    config = {
        "store-draft": HookConfig(
            url=f"http://127.0.0.1:{port}/webhook/store-email-draft",
            token="test-secret",
        ),
    }
    args = _make_args()
    run(args, config)

    thread.join(timeout=5)
    server.server_close()

    # Verify the server received the right payload
    assert _FakeWebhookHandler.last_payload["to"] == "alice@example.com"
    assert _FakeWebhookHandler.last_payload["subject"] == "Test subject"
    assert _FakeWebhookHandler.last_payload["body_plain"] == "Hello, world!"

    # Verify CLI printed the response
    captured = capsys.readouterr()
    assert '"status": "ok"' in captured.out
