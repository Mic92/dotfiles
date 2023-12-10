import json
import re
import subprocess
import sys
from pathlib import Path
from typing import Any, TypeVar

ROOT = Path(__file__).parent.parent.resolve()


class ConfigError(Exception):
    pass


def interpolate_secrets(option_val: str, secrets: dict[str, str]) -> str:
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
    if isinstance(val, float | int | str):
        val = interpolate_secrets(str(val), secrets)
        return [f"set {key}='{val}'"]

    if isinstance(val, list):
        return [f"add_list {key}='{interpolate_secrets(v, secrets)}'" for v in val]

    msg = f"{val} is not a string"
    raise ConfigError(msg)


def serialize_list_section(
    config_name: str,
    section_name: str,
    idx: int,
    list_obj: dict[str, str],
    secrets: dict[str, str],
) -> list[str]:
    lines = []
    _type = None

    # FIXME, how to delete all list sections?
    # truncate list so we can start from fresh

    _type = list_obj.get("_type")
    if _type is None:
        msg = f"{config_name}.@{section_name}[{idx}] has no type!"
        raise ConfigError(msg)
    del list_obj["_type"]
    lines.append(f"set {config_name}.@{section_name}[{idx}]={_type}")

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
    lines = []
    _type = None
    # truncate section so we can start from fresh
    lines.append(f"delete {config_name}.{section_name}")

    _type = section.get("_type")
    if _type is None:
        msg = f"{config_name}.{section_name} has no type"
        raise ConfigError(msg)
    del section["_type"]
    lines.append(f"set {config_name}.{section_name}={_type}")

    for option_name, option in section.items():
        # FIXME: how does escaping work?
        lines.extend(
            serialize_option_val(
                f"{config_name}.{section_name}.{option_name}",
                option,
                secrets,
            ),
        )
    return lines


def serialize_uci(configs: dict[str, Any], secrets: dict[str, str]) -> str:
    lines: list[str] = []
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
                # HACK: there seem no better way to clear out a list
                lines.extend(
                    f"delete {config_name}.@{section_name}[0]" for _i in range(10)
                )
                lines.extend(
                    f"add {config_name} {section_name}" for _i in range(len(section))
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
                # for option_name, option in section:
            elif isinstance(section, dict):
                lines.extend(
                    serialize_named_section(
                        config_name, section_name, section, secrets
                    ),
                )
            else:
                msg = f"{config_name}.{section_name} is not a valid section object, expected dict, got: {type(section).__name__}"
                raise ConfigError(
                    msg,
                )

    return "\n".join(lines)


T = TypeVar("T")


def ensure_type(parent: dict[str, Any], key: str, t: type[T]) -> T:
    val = parent.get(key, {})
    if not isinstance(val, t):
        msg = f"{key} is not of type: {t}"
        raise ConfigError(msg)
    return val


def ensure_dict(parent: dict[str, Any], key: str) -> dict:
    return ensure_type(parent, key, dict)


def load_sops_file(file: str) -> dict[str, str]:
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
    try:
        if len(sys.argv) < 2:
            print(f"USAGE: {sys.argv[0]} JSON_FILE")
            sys.exit(1)
        print(convert_file(Path(sys.argv[1])))

    except ConfigError as e:
        print(e, file=sys.stderr)
        sys.exit(1)
