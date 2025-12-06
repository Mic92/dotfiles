import logging
import os
import subprocess
from pathlib import Path
from typing import TYPE_CHECKING

import keyring

if TYPE_CHECKING:
    from collections.abc import Callable

# Set up logging to file so it doesn't interfere with pinentry protocol
cache_dir = (
    Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache")) / "rbw-pinentry"
)
cache_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
log_file = cache_dir / "pinentry.log"

logging.basicConfig(
    level=logging.WARNING,
    format="%(asctime)s - %(levelname)s - %(message)s",
    filename=str(log_file),
)
logger = logging.getLogger(__name__)


class Pinentry:
    """Pinentry wrapper that caches passwords in system secure storage."""

    def __init__(self) -> None:
        self.rbw_profile = os.environ.get("RBW_PROFILE", "rbw")
        self.service_name = "rbw-master-password"

    def _show_zenity_password_dialog(
        self,
        title: str = "",
        prompt: str = "",
        desc: str = "",
        error: str = "",
    ) -> str | None:
        try:
            zenity_cmd = ["zenity", "--password"]
            if title:
                zenity_cmd.extend(["--title", title])
            elif prompt:
                zenity_cmd.extend(["--title", prompt])

            # Build the text to display in the dialog
            text_parts: list[str] = []
            if error:
                text_parts.append(f"Error: {error}")
            if desc:
                text_parts.append(desc)
            if prompt and prompt != title:
                text_parts.append(prompt)

            if text_parts:
                zenity_cmd.extend(["--text", "\n\n".join(text_parts)])

            result = subprocess.run(
                zenity_cmd, capture_output=True, text=True, check=False
            )
            if result.returncode == 0 and result.stdout:
                password = result.stdout.strip()
                return password.rstrip("\n") if password else None
        except OSError as e:
            logger.warning("Failed to call zenity: %s", e)
        return None

    def _get_cached_password(self) -> str | None:
        """Get cached password from keyring."""
        try:
            return keyring.get_password(self.service_name, self.rbw_profile)
        except keyring.errors.KeyringError as e:
            logger.warning("Failed to get password: %s", e)
            return None

    def _clear_cached_password(self) -> None:
        """Clear cached password from keyring."""
        try:
            keyring.delete_password(self.service_name, self.rbw_profile)
        except keyring.errors.PasswordDeleteError:
            pass  # Password doesn't exist, which is fine
        except keyring.errors.KeyringError as e:
            logger.warning("Failed to delete password: %s", e)

    def _cache_password(self, password: str) -> None:
        """Cache password in keyring."""
        try:
            keyring.set_password(self.service_name, self.rbw_profile, password)
        except keyring.errors.KeyringError as e:
            logger.warning("Failed to store password: %s", e)

    def _handle_password(
        self, title: str, prompt: str, desc: str, error: str
    ) -> str | None:
        # If there was an auth error, clear the cached password
        if error:
            logger.warning("Authentication error: %s", error)
            self._clear_cached_password()
        else:
            # Try to return cached password
            cached_password = self._get_cached_password()
            if cached_password:
                return cached_password

        # Set defaults for dialog
        if not title:
            title = "rbw"
        if not desc:
            desc = (
                f"Authentication failed. Please enter the password for '{self.rbw_profile}'"
                if error
                else f"Please enter the password for '{self.rbw_profile}' (will be cached in secure storage)"
            )

        # Show prompt
        secret_value = self._show_zenity_password_dialog(
            title=title, prompt=prompt, desc=desc, error=error
        )

        # Cache password if successfully entered
        if secret_value:
            self._cache_password(secret_value)

        return secret_value

    def _set_state(self, state: dict[str, str], key: str, value: str) -> str:
        state[key] = value
        return "OK"

    def _handle_getpin(self, state: dict[str, str]) -> str:
        secret_value = self._handle_password(
            state["title"], state["prompt"], state["desc"], state["error"]
        )
        if secret_value:
            print(f"D {secret_value}", flush=True)
        return "OK"

    def _process_command(
        self, command: str, args: str, state: dict[str, str]
    ) -> str | None:
        command_handlers: dict[str, Callable[[], str]] = {
            "SETTITLE": lambda: self._set_state(state, "title", args),
            "SETDESC": lambda: self._set_state(state, "desc", args),
            "SETPROMPT": lambda: self._set_state(state, "prompt", args),
            "SETERROR": lambda: self._set_state(state, "error", args),
            "GETPIN": lambda: self._handle_getpin(state),
            "BYE": lambda: "BYE",
        }
        handler = command_handlers.get(command)
        return handler() if handler else "ERR Unknown command"

    def handle_pinentry_session(self) -> None:
        print("OK", flush=True)
        state: dict[str, str] = {"title": "", "prompt": "", "desc": "", "error": ""}
        while True:
            try:
                line = input()
            except EOFError:
                break
            parts = line.split(" ", 1)
            command = parts[0]
            args = parts[1] if len(parts) > 1 else ""
            response = self._process_command(command, args, state)
            if response == "BYE":
                break
            if response:
                print(response, flush=True)
