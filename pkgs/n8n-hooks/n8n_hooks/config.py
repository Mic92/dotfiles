"""Config loader — reads webhook URLs and a shared bearer token.

Expected format:

    {
      "token_command": "rbw get n8n-hooks-token",
      "hooks": {
        "store-draft": {
          "url": "https://n8n.thalheim.io/webhook/store-email-draft"
        }
      }
    }

A single ``token_command`` (or literal ``token``) is shared across all hooks.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass
class HookConfig:
    url: str
    token: str | None


def _xdg_config_home() -> Path:
    return Path(os.environ.get("XDG_CONFIG_HOME", Path.home() / ".config"))


def _resolve_token(raw: dict[str, object]) -> str | None:
    """Return the bearer token, running a command if needed."""
    if "token" in raw:
        return str(raw["token"])
    if "token_command" in raw:
        result = subprocess.run(
            str(raw["token_command"]),
            shell=True,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    return None


def load_config(path: str | None = None) -> dict[str, HookConfig]:
    """Load and parse the config file, returning per-hook configs."""
    if path is None:
        path_obj = _xdg_config_home() / "n8n-hooks" / "config.json"
    else:
        path_obj = Path(path)

    if not path_obj.exists():
        print(f"n8n-hooks: config not found: {path_obj}", file=sys.stderr)
        sys.exit(1)

    with open(path_obj) as f:
        raw: dict[str, object] = json.load(f)

    token = _resolve_token(raw)

    hooks_raw = raw.get("hooks", {})
    if not isinstance(hooks_raw, dict):
        print("n8n-hooks: 'hooks' must be an object", file=sys.stderr)
        sys.exit(1)

    configs: dict[str, HookConfig] = {}
    for name, entry in hooks_raw.items():
        if not isinstance(entry, dict):
            continue
        configs[name] = HookConfig(
            url=str(entry["url"]),
            token=token,
        )
    return configs
