"""Configuration utilities for vcal."""

from __future__ import annotations

import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Any


class ConfigError(Exception):
    """Configuration error."""


def ensure_type[T](value: Any, expected_type: type[T], field_name: str) -> T:  # noqa: ANN401
    """Ensure a value is of the expected type.

    Args:
        value: The value to check
        expected_type: The expected type (e.g., str, int, bool)
        field_name: Name of the field for error messages

    Returns:
        The value if it matches the type

    Raises:
        ConfigError: If type doesn't match

    """
    if not isinstance(value, expected_type):
        msg = f"'{field_name}' must be {expected_type.__name__}, got {type(value).__name__}"
        raise ConfigError(msg)
    return value


@dataclass
class UserConfig:
    """User configuration."""

    email: str
    name: str | None = None


@dataclass
class VcalConfig:
    """Complete vcal configuration."""

    user: UserConfig | None = None


def validate_config_data(data: dict[str, Any]) -> VcalConfig:
    """Validate and convert raw config data to VcalConfig.

    Raises:
        ConfigError: If configuration is invalid

    """
    config = VcalConfig()

    if "user" in data:
        user_data = ensure_type(data["user"], dict, "user")

        # Email is required in user section
        if "email" not in user_data:
            msg = "'email' is required in [user] section"
            raise ConfigError(msg)

        email = ensure_type(user_data["email"], str, "user.email")
        if not email or "@" not in email:
            msg = f"Invalid email address: {email}"
            raise ConfigError(msg)

        # Name is optional
        name = None
        if "name" in user_data:
            name = ensure_type(user_data["name"], str, "user.name")

        config.user = UserConfig(email=email, name=name)

    return config


def load_config() -> VcalConfig:
    """Load configuration from ~/.config/vcal/config.toml.

    Example config file:
    [user]
    email = "user@example.com"
    name = "User Name"  # Optional

    Returns:
        VcalConfig with loaded settings

    Raises:
        ConfigError: If config file exists but is invalid

    """
    config_file = Path.home() / ".config" / "vcal" / "config.toml"
    if not config_file.exists():
        return VcalConfig()

    try:
        with config_file.open("rb") as f:
            data = tomllib.load(f)
    except OSError as e:
        msg = f"Failed to read config file: {e}"
        raise ConfigError(msg) from e
    except tomllib.TOMLDecodeError as e:
        msg = f"Invalid TOML in config file: {e}"
        raise ConfigError(msg) from e

    return validate_config_data(data)


def load_config_or_exit() -> VcalConfig:
    """Load config, exit with error message if invalid.

    This is a convenience function for CLI usage.
    """
    try:
        return load_config()
    except ConfigError as e:
        print(f"Error in ~/.config/vcal/config.toml: {e}", file=sys.stderr)
        sys.exit(1)
