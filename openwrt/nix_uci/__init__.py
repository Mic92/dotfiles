"""Module for converting JSON configuration to UCI (Unified Configuration Interface) format."""

import json
import re
import subprocess
import sys
from pathlib import Path
from typing import Any, TypeVar

ROOT = Path(__file__).parent.parent.resolve()


class ConfigError(Exception):
    """Exception raised for configuration errors."""


def interpolate_secrets(option_val: str, secrets: dict[str, str]) -> str:
    """Replace secret placeholders in option values with actual secret values."""

    def substitute_secrets(matchobj: re.Match) -> str:
        name = matchobj.group(1)
        val = secrets.get(name)
        if val:
            return str(val)
        msg = f"Tried to use secret {name}, but no secret with this name specified."
        raise ConfigError(
            msg,
        )

    return re.sub("@(.+)@", substitute_secrets, option_val)


def serialize_option_val(
    key: str,
    val: list | str,
    secrets: dict[str, str],
) -> list[str]:
    """Serialize an option value to UCI format."""
    if isinstance(val, float | int | str):
        val = interpolate_secrets(str(val), secrets)
        return [f"uci set {key}='{val}'"]

    if isinstance(val, list):
        return [f"uci add_list {key}='{interpolate_secrets(v, secrets)}'" for v in val]

    msg = f"{val} is not a string"
    raise ConfigError(msg)


def serialize_list_section(
    config_name: str,
    section_name: str,
    idx: int,
    list_obj: dict[str, str],
    secrets: dict[str, str],
) -> list[str]:
    """Serialize a list section to UCI format."""
    lines = []
    _type = None

    _type = list_obj.get("_type")
    if _type is None:
        msg = f"{config_name}.@{section_name}[{idx}] has no type!"
        raise ConfigError(msg)
    del list_obj["_type"]
    lines.append(f"uci set {config_name}.@{section_name}[{idx}]={_type}")

    for option_name, option in list_obj.items():
        lines.extend(
            serialize_option_val(
                f"{config_name}.@{section_name}[{idx}].{option_name}",
                option,
                secrets,
            ),
        )
    return lines


def serialize_named_section(
    config_name: str,
    section_name: str,
    section: dict[str, str],
    secrets: dict[str, str],
) -> list[str]:
    """Serialize a named section to UCI format."""
    lines = []
    _type = None
    # truncate section so we can start from fresh
    lines.append(f"uci delete {config_name}.{section_name}")

    _type = section.get("_type")
    if _type is None:
        msg = f"{config_name}.{section_name} has no type"
        raise ConfigError(msg)
    del section["_type"]
    lines.append(f"uci set {config_name}.{section_name}={_type}")

    for option_name, option in section.items():
        # TODO(@joerg): Investigate how escaping works in UCI
        lines.extend(
            serialize_option_val(
                f"{config_name}.{section_name}.{option_name}",
                option,
                secrets,
            ),
        )
    return lines


def serialize_uci(configs: dict[str, Any], secrets: dict[str, str]) -> str:
    """Serialize configuration dictionary to UCI format."""
    lines: list[str] = []
    # Add shebang to make it a proper shell script
    lines.append("#!/bin/sh")
    lines.append("set -e")
    lines.append("")

    config_names = []
    for config_name, sections in configs.items():
        config_names.append(config_name)
        if not isinstance(sections, dict):
            msg = f"{config_name} is not a valid config object, expected dict, got: {type(sections).__name__}"
            raise ConfigError(
                msg,
            )

        for section_name, section in sections.items():
            if isinstance(section, list):
                # Use shell loop to properly delete all existing list sections
                lines.append(f"# Clear all existing {config_name}.@{section_name} sections")
                lines.append(f"while uci -q delete {config_name}.@{section_name}[-1] 2>/dev/null; do :; done")
                lines.append("")

                # Add new sections
                lines.extend(
                    f"uci add {config_name} {section_name}" for _i in range(len(section))
                )

                for idx, list_obj in enumerate(section):
                    lines.extend(
                        serialize_list_section(
                            config_name,
                            section_name,
                            idx,
                            list_obj,
                            secrets,
                        ),
                    )
                lines.append("")
            elif isinstance(section, dict):
                lines.extend(
                    serialize_named_section(
                        config_name,
                        section_name,
                        section,
                        secrets,
                    ),
                )
                lines.append("")
            else:
                msg = f"{config_name}.{section_name} is not a valid section object, expected dict, got: {type(section).__name__}"
                raise ConfigError(
                    msg,
                )

    # Commit all changes at the end
    lines.append("uci commit")

    return "\n".join(lines)


T = TypeVar("T")


def ensure_type[T](parent: dict[str, Any], key: str, t: type[T]) -> T:
    """Ensure a dictionary value has the expected type."""
    val = parent.get(key, {})
    if not isinstance(val, t):
        msg = f"{key} is not of type: {t}"
        raise ConfigError(msg)
    return val


def ensure_dict(parent: dict[str, Any], key: str) -> dict:
    """Ensure a dictionary value is a dict."""
    return ensure_type(parent, key, dict)


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
    return json.loads(res.stdout)


def convert_file(path: Path) -> str:
    """Convert a JSON configuration file to UCI format."""
    uci_json = json.load(path.open())
    if not isinstance(uci_json, dict):
        msg = "json file has no settings attribute set"
        raise ConfigError(msg)

    settings = ensure_dict(uci_json, "settings")
    if not settings:
        msg = "json file has no settings attribute set"
        raise ConfigError(msg)

    uci_json.get("secrets", {})
    sops_files = ensure_type(
        ensure_dict(ensure_dict(uci_json, "secrets"), "sops"),
        "files",
        list,
    )
    secrets = {}
    for sops_file in sops_files:
        secrets.update(load_sops_file(sops_file))

    return serialize_uci(settings, secrets)


def main() -> None:
    """Execute main entry point for the CLI."""
    try:
        min_args = 2
        if len(sys.argv) < min_args:
            print(f"USAGE: {sys.argv[0]} JSON_FILE")
            sys.exit(1)
        print(convert_file(Path(sys.argv[1])))

    except ConfigError as e:
        print(e, file=sys.stderr)
        sys.exit(1)
