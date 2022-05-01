#!/usr/bin/env python

import subprocess
import sys
from typing import Any, Dict, List, Type
from pathlib import Path
import json
import re

ROOT = Path(__file__).parent.parent.resolve()


class ConfigError(Exception):
    pass


def interpolate_secrets(option_val: str, secrets: Dict[str, str]) -> str:
    def substitute_secrets(matchobj: re.Match) -> str:
        name = matchobj.group(1)
        val = secrets.get(name)
        if val:
            return str(val)
        raise ConfigError(
            f"Tried to use secret {name}, but no secret with this name specified."
        )

    return re.sub("@(.+)@", substitute_secrets, option_val)


def serialize_list_section(
    config_name: str,
    section_name: str,
    idx: int,
    list_obj: Dict[str, str],
    secrets: Dict[str, str],
) -> List[str]:
    lines = []
    _type = None

    # FIXME, how to delete all list sections?
    # truncate list so we can start from fresh

    for option_name, option in list_obj.items():
        if not isinstance(option, str):
            raise ConfigError(
                f"{config_name}.{section_name}.{option_name} is not a string"
            )

        if option_name == "_type":
            _type = option
            lines.append(f"set {config_name}.@{section_name}[{idx}]={option}")
        else:
            interpolated = interpolate_secrets(option, secrets)
            # FIXME: how does escaping work?
            lines.append(
                f"set {config_name}.@{section_name}[{idx}].{option_name}='{interpolated}'"
            )
    if _type is None:
        raise ConfigError(f"{config_name}.{section_name} has no type")
    return lines


def serialize_named_section(
    config_name: str,
    section_name: str,
    section: Dict[str, str],
    secrets: Dict[str, str],
) -> List[str]:
    lines = []
    _type = None
    # truncate section so we can start from fresh
    lines.append(f"delete {config_name}.{section_name}")

    for option_name, option in section.items():
        if not isinstance(option, str):
            raise ConfigError(
                f"{config_name}.{section_name}.{option_name} is not a string"
            )
        if option_name == "_type":
            _type = option
            lines.append(f"set {config_name}.{section_name}={option}")
        else:
            interpolated = interpolate_secrets(option, secrets)
            # FIXME: how does escaping work?
            lines.append(
                f"set {config_name}.{section_name}.{option_name}='{interpolated}'"
            )
    if _type is None:
        raise ConfigError(f"{config_name}.{section_name} has no type")
    return lines


def serialize_uci(configs: Dict[str, Any], secrets: Dict[str, str]) -> str:
    lines = []
    config_names = []
    for config_name, sections in configs.items():
        config_names.append(config_name)
        if not isinstance(sections, Dict):
            raise ConfigError(
                f"{config_name} is not a valid config object, expected dict, got: {type(sections).__name__}"
            )

        for section_name, section in sections.items():
            if isinstance(section, List):
                # HACK: there seem no better way to clear out a list
                for i in range(10):
                    lines.append(f"delete {config_name}.@{section_name}[{i}]")
                for i in range(len(section)):
                    lines.append(f"add {config_name} {section_name}")

                for idx, list_obj in enumerate(section):
                    lines.extend(
                        serialize_list_section(
                            config_name, section_name, idx, list_obj, secrets
                        )
                    )
                # for option_name, option in section:
            elif isinstance(section, Dict):
                lines.extend(
                    serialize_named_section(config_name, section_name, section, secrets)
                )
            else:
                raise ConfigError(
                    f"{config_name}.{section_name} is not a valid section object, expected dict, got: {type(section).__name__}"
                )

    return "\n".join(lines)

def ensure_type(parent: Dict[str, Any], key: str, t: Type) -> Dict:
    val = parent.get(key, {})
    if not isinstance(val, t):
        raise ConfigError(f"{key} is not of type: {t}")
    return val

def ensure_dict(parent: Dict[str, Any], key: str) -> Dict:
    return ensure_type(parent, key, dict)


def load_sops_file(file: str) -> Dict[str, str]:
    res = subprocess.run(
        ["sops", "-d", "--output-type", "json", file],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    if res.returncode != 0:
        raise ConfigError(f"Cannot decrypt '{file}' with sops:\n{res.stderr}")
    return json.loads(res.stdout)


def convert_file(path: str) -> str:
    with open(path) as f:
        uci_json = json.load(f)
    if not isinstance(uci_json, Dict):
        raise ConfigError("json file has no settings attribute set")

    settings = ensure_dict(uci_json, "settings")
    if not settings:
        raise ConfigError("json file has no settings attribute set")

    uci_json.get("secrets", {})
    sops_files = ensure_type(
        ensure_dict(ensure_dict(uci_json, "secrets"), "sops"), "files", list
    )
    secrets = {}
    for sops_file in sops_files:
        secrets.update(load_sops_file(sops_file))

    return serialize_uci(settings, secrets)


def main() -> None:
    try:
        if len(sys.argv) < 2:
            print(f"USAGE: {sys.argv[0]} JSON_FILE")
            sys.exit(1)
        print(convert_file(sys.argv[1]))

    except ConfigError as e:
        print(e, file=sys.stderr)
        sys.exit(1)
