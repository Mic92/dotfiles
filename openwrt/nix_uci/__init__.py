"""Render a nix-generated OpenWrt configuration into a self-contained ucode program.

The program (apply.uc with the configuration embedded) is piped to `ucode -`
on the device, which applies it through the uci API instead of `uci batch`.
"""

import json
import re
import subprocess
import sys
from importlib.resources import files
from pathlib import Path
from typing import Any, cast

type UciValue = str | int | float | bool | list[UciValue] | dict[str, UciValue] | None

SECRET_RE = re.compile(r"@([A-Za-z0-9_]+)@")


class ConfigError(Exception):
    """Exception raised for configuration errors."""


def load_sops_file(file: str) -> dict[str, str]:
    """Load and decrypt a SOPS-encrypted file."""
    res = subprocess.run(
        ["sops", "-d", "--output-type", "json", file],
        capture_output=True,
        text=True,
        check=False,
    )
    if res.returncode != 0:
        msg = f"Cannot decrypt '{file}' with sops:\n{res.stderr}"
        raise ConfigError(msg)
    return cast("dict[str, str]", json.loads(res.stdout))


def get_secret(secrets: dict[str, str], name: str) -> str:
    """Look up a secret, failing loudly on typos."""
    if name not in secrets:
        msg = f"Tried to use secret {name}, but no secret with this name specified."
        raise ConfigError(msg)
    return str(secrets[name])


def interpolate(val: UciValue, secrets: dict[str, str]) -> UciValue:
    """Replace @name@ placeholders in all strings of a uci value."""
    if isinstance(val, str):
        return SECRET_RE.sub(lambda m: get_secret(secrets, m.group(1)), val)
    if isinstance(val, list):
        return [interpolate(v, secrets) for v in val]
    if isinstance(val, dict):
        return {k: interpolate(v, secrets) for k, v in val.items()}
    return val


def collect_files(
    spec: dict[str, dict[str, Any]],
    secrets: dict[str, str],
) -> tuple[list[str], list[dict[str, Any]]]:
    """Resolve file specs into (directories to replace, files with content)."""
    replace_dirs: list[str] = []
    out: list[dict[str, Any]] = []
    for path, f in sorted(spec.items()):
        mode = int(f["mode"], 8)
        if f.get("secret") is not None:
            out.append(
                {
                    "path": path,
                    "mode": mode,
                    "content": get_secret(secrets, f["secret"]),
                },
            )
            continue
        source = f.get("source")
        if source is None:
            msg = f"openwrt.files.{path}: one of text, source or secret must be set"
            raise ConfigError(msg)
        src = Path(source)
        if src.is_dir():
            replace_dirs.append(path)
            out.extend(
                {
                    "path": f"{path}/{p.relative_to(src)}",
                    "mode": mode,
                    "content": p.read_text(),
                }
                for p in sorted(src.rglob("*"))
                if p.is_file()
            )
        else:
            out.append({"path": path, "mode": mode, "content": src.read_text()})
    return replace_dirs, out


def render(config: dict[str, Any]) -> str:
    """Render the full ucode program for a configuration."""
    secrets: dict[str, str] = {}
    for sops_file in config["uci"]["secrets"]["sops"]["files"]:
        secrets.update(load_sops_file(sops_file))

    openwrt = config["openwrt"]
    replace_dirs, file_list = collect_files(openwrt["files"], secrets)
    root_password = openwrt.get("rootPasswordSecret")
    payload = {
        "packages": openwrt["packages"],
        "replace_dirs": replace_dirs,
        "files": file_list,
        "settings": interpolate(config["uci"]["settings"], secrets),
        "root_password": None
        if root_password is None
        else get_secret(secrets, root_password),
        "restart_services": openwrt["restartServices"],
    }
    applier = files(__package__).joinpath("apply.uc").read_text()
    # 'use strict' must stay the first statement, so declare cfg after it.
    head, sep, rest = applier.partition("'use strict';\n")
    return f"{head}{sep}const cfg = {json.dumps(payload)};\n{rest}"


def main() -> None:
    """Execute main entry point for the CLI."""
    if len(sys.argv) != 2:  # noqa: PLR2004
        print(f"USAGE: {sys.argv[0]} CONFIG_JSON", file=sys.stderr)
        sys.exit(1)
    try:
        print(render(json.loads(Path(sys.argv[1]).read_text())))
    except ConfigError as e:
        print(e, file=sys.stderr)
        sys.exit(1)
