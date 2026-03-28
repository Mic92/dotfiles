"""Tests for the github hook."""

from __future__ import annotations

import argparse
import json
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from threading import Thread
from typing import Any

import pytest

from n8n_hooks.config import HookConfig
from n8n_hooks.hooks.github import _discover, build_payload, run


def _args(**kw: Any) -> argparse.Namespace:
    defaults: dict[str, Any] = {"path": "/user", "term": "", "query": []}
    defaults.update(kw)
    return argparse.Namespace(**defaults)


def test_payload_path_and_query() -> None:
    p = build_payload(
        _args(path="/repos/Mic92/dotfiles/issues", query=["state=open", "per_page=5"])
    )
    assert p == {
        "path": "/repos/Mic92/dotfiles/issues",
        "query": {"state": "open", "per_page": "5"},
    }


def test_payload_no_method_no_body() -> None:
    # Read-only hook: payload must never carry method/body even if someone
    # tries to sneak them into the namespace.
    ns = _args(path="/user")
    ns.method = "DELETE"
    ns.data = '{"x":1}'
    p = build_payload(ns)
    assert p == {"path": "/user"}


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


def test_run_prints_body_and_exits_on_error(
    capsys: pytest.CaptureFixture[str],
) -> None:
    _FakeWebhook.reply = {"status": 404, "body": {"message": "Not Found"}}
    srv = HTTPServer(("127.0.0.1", 0), _FakeWebhook)
    port = srv.server_address[1]
    Thread(target=srv.handle_request, daemon=True).start()

    cfg = {"github": HookConfig(url=f"http://127.0.0.1:{port}/w", token=None)}
    with pytest.raises(SystemExit):
        run(_args(path="/nope"), cfg)
    srv.server_close()

    assert _FakeWebhook.last == {"path": "/nope"}
    assert "Not Found" in capsys.readouterr().out


# ---------- discover: OpenAPI 3.0 parsing ----------

# Minimal OpenAPI 3.0 slice — exercises $ref in components/parameters,
# schema.type nesting, and requestBody (vs Swagger 2's body-in-params).
SPEC: dict[str, Any] = {
    "components": {
        "parameters": {
            "owner": {
                "name": "owner",
                "in": "path",
                "required": True,
                "schema": {"type": "string"},
            },
        },
    },
    "paths": {
        "/repos/{owner}/{repo}/issues": {
            "get": {
                "summary": "List repository issues",
                "parameters": [
                    {"$ref": "#/components/parameters/owner"},
                    {
                        "name": "state",
                        "in": "query",
                        "schema": {"type": "string"},
                    },
                ],
            },
            "post": {
                "summary": "Create an issue",
                "parameters": [],
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "type": "object",
                                "required": ["title"],
                                "properties": {
                                    "title": {"type": "string"},
                                    "labels": {
                                        "type": "array",
                                        "items": {"type": "string"},
                                    },
                                },
                            }
                        }
                    }
                },
            },
        },
        "/user": {"get": {"summary": "Get the authenticated user"}},
    },
}


def _cache_spec(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("XDG_CACHE_HOME", str(tmp_path))
    d = tmp_path / "n8n-hooks"
    d.mkdir()
    (d / "github-api.json").write_text(json.dumps(SPEC))


def test_discover_grep(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _cache_spec(tmp_path, monkeypatch)
    _discover("issue")
    out = capsys.readouterr().out
    assert "/repos/{owner}/{repo}/issues [get,post]" in out
    assert "/user" not in out


def test_discover_detail(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _cache_spec(tmp_path, monkeypatch)
    _discover("/repos/{owner}/{repo}/issues")
    out = capsys.readouterr().out
    assert "GET /repos/{owner}/{repo}/issues" in out
    assert "* owner (path:string)" in out  # $ref resolved
    assert "state (query:string)" in out
    assert "POST" in out and "not available via this hook" in out
    assert "* title: string" in out  # requestBody schema
    assert "labels: [string]" in out  # array rendering


def test_run_discover_skips_webhook(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    _cache_spec(tmp_path, monkeypatch)
    run(_args(path="discover", term="user"), {})
    assert "/user [get]" in capsys.readouterr().out
