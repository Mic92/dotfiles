"""Tests for the gitea hook."""

from __future__ import annotations

import argparse
import io
import json
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from threading import Thread
from typing import Any
from unittest.mock import patch

import pytest

from n8n_hooks.config import HookConfig
from n8n_hooks.hooks.gitea import _discover, _fmt_schema, build_payload, run


def _args(**kw: Any) -> argparse.Namespace:
    defaults: dict[str, Any] = {
        "method": "GET",
        "path": "/user",
        "data": None,
        "query": [],
    }
    defaults.update(kw)
    return argparse.Namespace(**defaults)


# ---------- payload building: the three -d input modes ----------


def test_payload_inline_body_and_query() -> None:
    p = build_payload(
        _args(
            method="POST",
            path="/user/repos",
            data='{"name":"x"}',
            query=["limit=50", "page=2"],
        )
    )
    assert p == {
        "method": "POST",
        "path": "/user/repos",
        "body": {"name": "x"},
        "query": {"limit": "50", "page": "2"},
    }


def test_payload_body_from_file(tmp_path: Path) -> None:
    f = tmp_path / "body.json"
    f.write_text('{"name":"from-file"}')
    p = build_payload(_args(method="POST", data=f"@{f}"))
    assert p["body"] == {"name": "from-file"}


def test_payload_body_from_stdin() -> None:
    with patch("sys.stdin", io.StringIO('{"name":"from-stdin"}')):
        p = build_payload(_args(method="POST", data="-"))
    assert p["body"] == {"name": "from-stdin"}


# ---------- discover: swagger parsing ----------

# Minimal but realistic slice of the Gitea swagger spec — exercises
# $ref resolution, required markers, array types, and grep/detail modes.
SPEC: dict[str, Any] = {
    "definitions": {
        "CreateRepoOption": {
            "type": "object",
            "required": ["name"],
            "properties": {
                "name": {"type": "string", "description": "repo name"},
                "private": {"type": "boolean"},
                "topics": {"type": "array", "items": {"type": "string"}},
            },
        },
    },
    "paths": {
        "/user/repos": {
            "get": {
                "summary": "List repos",
                "parameters": [
                    {"name": "limit", "in": "query", "type": "integer"},
                ],
            },
            "post": {
                "summary": "Create a repository",
                "parameters": [
                    {
                        "name": "body",
                        "in": "body",
                        "schema": {"$ref": "#/definitions/CreateRepoOption"},
                    },
                ],
            },
        },
        "/repos/{owner}/{repo}": {
            "delete": {
                "summary": "Delete a repository",
                "parameters": [
                    {"name": "owner", "in": "path", "required": True, "type": "string"},
                ],
            },
        },
    },
}


def _fake_urlopen(*_a: Any, **_kw: Any) -> io.BytesIO:
    return io.BytesIO(json.dumps(SPEC).encode())


def test_fmt_schema_required_and_arrays(
    capsys: pytest.CaptureFixture[str],
) -> None:
    _fmt_schema(SPEC, {"$ref": "#/definitions/CreateRepoOption"}, "  ")
    out = capsys.readouterr().out
    assert "* name: string" in out  # required marker
    assert "  private: boolean" in out  # optional
    assert "topics: [string]" in out  # array rendering


def test_discover_grep(capsys: pytest.CaptureFixture[str]) -> None:
    with patch("urllib.request.urlopen", _fake_urlopen):
        _discover("repo")
    out = capsys.readouterr().out
    assert "/user/repos [get,post]" in out
    assert "/repos/{owner}/{repo} [delete]" in out


def test_discover_detail(capsys: pytest.CaptureFixture[str]) -> None:
    with patch("urllib.request.urlopen", _fake_urlopen):
        _discover("/user/repos")
    out = capsys.readouterr().out
    assert "GET /user/repos" in out
    assert "POST /user/repos" in out
    assert "limit (query:integer)" in out
    assert "* name: string" in out  # body schema expanded via $ref


# ---------- run() behaviour ----------


class _FakeWebhook(BaseHTTPRequestHandler):
    last: dict[str, Any] = {}
    reply: dict[str, Any] = {}

    def do_POST(self) -> None:
        length = int(self.headers.get("Content-Length", 0))
        _FakeWebhook.last = json.loads(self.rfile.read(length))
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps(_FakeWebhook.reply).encode())

    def log_message(self, *_a: Any) -> None:
        pass


def test_run_exits_nonzero_on_api_error(
    capsys: pytest.CaptureFixture[str],
) -> None:
    _FakeWebhook.reply = {"status": 404, "body": {"message": "not found"}}
    srv = HTTPServer(("127.0.0.1", 0), _FakeWebhook)
    port = srv.server_address[1]
    Thread(target=srv.handle_request, daemon=True).start()

    cfg = {"gitea": HookConfig(url=f"http://127.0.0.1:{port}/w", token=None)}
    with pytest.raises(SystemExit):
        run(_args(method="GET", path="/nope"), cfg)
    srv.server_close()

    assert _FakeWebhook.last == {"method": "GET", "path": "/nope"}
    assert "not found" in capsys.readouterr().out


def test_run_discover_skips_webhook(capsys: pytest.CaptureFixture[str]) -> None:
    # discover must work without any n8n config
    with patch("urllib.request.urlopen", _fake_urlopen):
        run(_args(method="discover", path="repo"), {})
    assert "/user/repos" in capsys.readouterr().out
