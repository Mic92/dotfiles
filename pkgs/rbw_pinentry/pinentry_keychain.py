"""
rbw pinentry wrapper with secure storage backend.

Stores and retrieves master passwords from system secure storage.
Supports macOS Keychain and Linux Secret Service (KDE Wallet, GNOME Keyring).
Detects password validation failures through SETERROR messages.
"""

import logging
import os
import platform
import subprocess
import sys
import time
from abc import ABC, abstractmethod
from pathlib import Path

# Set up logging to file so it doesn't interfere with pinentry protocol
# Use XDG_CACHE_HOME or fallback to ~/.cache
cache_dir = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache")) / "rbw-pinentry"
cache_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
log_file = cache_dir / "pinentry.log"

logging.basicConfig(
    level=logging.WARNING,
    format="%(asctime)s - %(levelname)s - %(message)s",
    filename=str(log_file),
)
logger = logging.getLogger(__name__)


class SecretBackendError(Exception):
    """Exception for secret backend operations."""


class SecretBackend(ABC):
    """Abstract base class for secure password storage backends."""

    @abstractmethod
    def get_password(self, service_name: str, account: str) -> str | None:
        """Retrieve password from secure storage.

        Args:
            service_name: Service identifier
            account: Account/username identifier

        Returns:
            Password string if found, None otherwise
        """

    @abstractmethod
    def store_password(self, service_name: str, account: str, password: str) -> bool:
        """Store password in secure storage.

        Args:
            service_name: Service identifier
            account: Account/username identifier
            password: Password to store

        Returns:
            True if successful, False otherwise
        """

    @abstractmethod
    def delete_password(self, service_name: str, account: str) -> bool:
        """Delete password from secure storage.

        Args:
            service_name: Service identifier
            account: Account/username identifier

        Returns:
            True if successful, False otherwise
        """


class MacOSKeychainBackend(SecretBackend):
    """macOS Keychain backend using the security command."""

    def get_password(self, service_name: str, account: str) -> str | None:
        """Retrieve password from macOS Keychain."""
        try:
            result = subprocess.run(
                [
                    "security",
                    "find-generic-password",
                    "-a",
                    account,
                    "-s",
                    service_name,
                    "-w",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            if result.returncode == 0:
                return result.stdout.strip()
        except OSError as e:
            logger.warning("Failed to read from keychain: %s", e)
        return None

    def store_password(self, service_name: str, account: str, password: str) -> bool:
        """Store password in macOS Keychain."""
        try:
            # Use -U to update if exists
            subprocess.run(
                [
                    "security",
                    "add-generic-password",
                    "-a",
                    account,
                    "-s",
                    service_name,
                    "-w",
                    password,
                    "-D",
                    "application password",
                    "-U",  # Update if exists
                    "-T",
                    "",  # Allow access without prompting
                ],
                capture_output=True,
                check=False,
            )
        except OSError as e:
            logger.warning("Failed to store in keychain: %s", e)
            return False
        else:
            return True

    def delete_password(self, service_name: str, account: str) -> bool:
        """Delete password from macOS Keychain."""
        try:
            subprocess.run(
                [
                    "security",
                    "delete-generic-password",
                    "-a",
                    account,
                    "-s",
                    service_name,
                    "-D",
                    "application password",
                ],
                capture_output=True,
                check=False,
            )
        except OSError as e:
            logger.warning("Failed to delete from keychain: %s", e)
            return False
        else:
            return True


class LinuxSecretServiceBackend(SecretBackend):
    """Linux Secret Service backend (KDE Wallet, GNOME Keyring)."""

    def __init__(self) -> None:
        """Initialize the Secret Service backend."""
        try:
            import secretstorage  # noqa: PLC0415

            self._secretstorage = secretstorage
        except ImportError as e:
            msg = "secretstorage module not available. Install python3-secretstorage."
            raise SecretBackendError(msg) from e

    def get_password(self, service_name: str, account: str) -> str | None:
        """Retrieve password from Secret Service."""
        try:
            with self._secretstorage.dbus_init() as connection:
                collection = self._secretstorage.get_default_collection(connection)
                items = collection.search_items(
                    {"service": service_name, "account": account}
                )
                for item in items:
                    # Return the first matching item
                    secret = item.get_secret()
                    if isinstance(secret, bytes):
                        return secret.decode("utf-8")
                    return str(secret)
                return None
        except (OSError, RuntimeError) as e:
            logger.warning("Failed to read from Secret Service: %s", e)
            return None
        except self._secretstorage.exceptions.LockedException as e:
            logger.warning("Secret Service is locked: %s", e)
            return None

    def store_password(self, service_name: str, account: str, password: str) -> bool:
        """Store password in Secret Service."""
        try:
            with self._secretstorage.dbus_init() as connection:
                collection = self._secretstorage.get_default_collection(connection)
                # Delete existing items first
                items = collection.search_items(
                    {"service": service_name, "account": account}
                )
                for item in items:
                    item.delete()
                # Create new item
                collection.create_item(
                    f"rbw master password for {account}",
                    {"service": service_name, "account": account},
                    password.encode("utf-8"),
                    replace=True,
                )
        except (OSError, RuntimeError) as e:
            logger.warning("Failed to store in Secret Service: %s", e)
            return False
        except self._secretstorage.exceptions.LockedException as e:
            logger.warning("Secret Service is locked: %s", e)
            return False
        else:
            return True

    def delete_password(self, service_name: str, account: str) -> bool:
        """Delete password from Secret Service."""
        try:
            with self._secretstorage.dbus_init() as connection:
                collection = self._secretstorage.get_default_collection(connection)
                items = collection.search_items(
                    {"service": service_name, "account": account}
                )
                for item in items:
                    item.delete()
        except (OSError, RuntimeError) as e:
            logger.warning("Failed to delete from Secret Service: %s", e)
            return False
        except self._secretstorage.exceptions.LockedException as e:
            logger.warning("Secret Service is locked: %s", e)
            return False
        else:
            return True


def get_backend() -> SecretBackend:
    """Get the appropriate backend for the current platform."""
    system = platform.system()
    if system == "Darwin":
        return MacOSKeychainBackend()
    # Assume Secret Service works on all non-Darwin platforms
    return LinuxSecretServiceBackend()


class PinentryKeychain:
    """Pinentry wrapper that caches passwords in system secure storage."""

    def __init__(self) -> None:
        """Initialize the pinentry keychain wrapper."""
        self.rbw_profile = os.environ.get("RBW_PROFILE", "rbw")
        self.service_name = "rbw-master-password"
        self.backend = get_backend()
        # Use XDG_CACHE_HOME or fallback to ~/.cache
        cache_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
        self.cache_state_file = cache_dir / f"{self.rbw_profile}-cache-state"

    def mark_cache_used(self) -> None:
        """Mark that we just returned a cached password."""
        try:
            self.cache_state_file.write_text(str(time.time()))
            # Ensure only user can read
            self.cache_state_file.chmod(0o600)
        except OSError as e:
            logger.warning("Failed to mark cache state: %s", e)

    def was_cache_used_recently(self) -> bool:
        """Check if we recently returned a cached password (within 2 seconds)."""
        try:
            if self.cache_state_file.exists():
                timestamp = float(self.cache_state_file.read_text())
                # If we returned a cached password within the last 2 seconds,
                # this error is likely due to that cached password being wrong
                return time.time() - timestamp < 2
        except (OSError, ValueError) as e:
            logger.warning("Failed to read cache state: %s", e)
        return False

    def clear_cache_state(self) -> None:
        """Clear the cache state file."""
        try:
            if self.cache_state_file.exists():
                self.cache_state_file.unlink()
        except OSError as e:
            logger.warning("Failed to clear cache state: %s", e)

    def _show_zenity_password_dialog(
        self,
        title: str = "",
        prompt: str = "",
        desc: str = "",
        error: str = "",
    ) -> str | None:
        """Show a password dialog using zenity and return the password."""
        try:
            # Build zenity command
            zenity_cmd = ["zenity", "--password"]

            # Set title if provided
            if title:
                zenity_cmd.extend(["--title", title])
            elif prompt:
                zenity_cmd.extend(["--title", prompt])

            # Build the window text from description and error
            window_text_parts = []
            if error:
                window_text_parts.append(f"Error: {error}")
            if desc:
                window_text_parts.append(desc)
            if prompt and prompt != title:
                window_text_parts.append(prompt)

            window_text = "\n\n".join(window_text_parts) if window_text_parts else prompt or "Enter password:"

            if window_text:
                zenity_cmd.extend(["--text", window_text])

            # Run zenity and capture password
            result = subprocess.run(
                zenity_cmd,
                capture_output=True,
                text=True,
                check=False,
            )

            # Zenity returns 0 on success, non-zero on cancel/error
            if result.returncode == 0 and result.stdout:
                password = result.stdout.strip()
                # Remove trailing newline that zenity might add
                return password.rstrip("\n") if password else None

        except OSError as e:
            logger.warning("Failed to call zenity: %s", e)
        return None

    def _handle_master_password(
        self,
        error: str,
    ) -> str | None:
        """Handle master password request with caching logic."""
        # Check if we have an error
        if error:
            logger.warning("Authentication error: %s", error)
            # Check if this error is from a cached password
            if self.was_cache_used_recently():
                # The cached password was wrong, clear it
                self.backend.delete_password(self.service_name, self.rbw_profile)
                self.clear_cache_state()
                # Force re-prompt
                cached_password = None
            else:
                # User entered wrong password manually, don't clear cache
                # but do re-prompt
                cached_password = None
        else:
            # No error, try to get from keychain
            cached_password = self.backend.get_password(
                self.service_name, self.rbw_profile
            )

        if cached_password:
            # Mark that we're using cached password
            self.mark_cache_used()
            return cached_password

        # Need to prompt for password
        self.clear_cache_state()  # Not using cache

        # Build dialog parameters
        title = "rbw"
        prompt = "Master Password"
        if error:
            desc = f"Authentication failed. Please enter the master password for '{self.rbw_profile}'"
        else:
            desc = f"Please enter the master password for '{self.rbw_profile}' (will be cached in secure storage)"

        secret_value = self._show_zenity_password_dialog(
            title=title,
            prompt=prompt,
            desc=desc,
            error=error,
        )

        # Store in secure storage if we got a password and there was no error
        # We can't validate it here (would cause recursion), but if it's wrong,
        # the next call will have SETERROR set and we'll clear it
        if secret_value and not error:
            self.backend.store_password(
                self.service_name, self.rbw_profile, secret_value
            )

        return secret_value

    def _handle_other_password(
        self,
        title: str,
        prompt: str,
        desc: str,
        error: str,
    ) -> str | None:
        """Handle non-master password prompts."""
        return self._show_zenity_password_dialog(
            title=title,
            prompt=prompt,
            desc=desc,
            error=error,
        )

    def _process_command(
        self,
        command: str,
        args: str,
        state: dict[str, str],
    ) -> str | None:
        """Process a single pinentry command."""
        # Map commands to their handlers
        command_handlers = {
            "SETTITLE": lambda: self._set_state(state, "title", args),
            "SETDESC": lambda: self._set_state(state, "desc", args),
            "SETPROMPT": lambda: self._set_state(state, "prompt", args),
            "SETERROR": lambda: self._set_state(state, "error", args),
            "GETPIN": lambda: self._handle_getpin(state),
            "BYE": lambda: "BYE",
        }

        handler = command_handlers.get(command)
        return handler() if handler else "ERR Unknown command"

    def _set_state(self, state: dict[str, str], key: str, value: str) -> str:
        """Set a state value and return OK."""
        state[key] = value
        return "OK"

    def _handle_getpin(self, state: dict[str, str]) -> str:
        """Handle GETPIN command."""
        if state["prompt"] == "Master Password":
            secret_value = self._handle_master_password(state["error"])
        else:
            secret_value = self._handle_other_password(
                state["title"],
                state["prompt"],
                state["desc"],
                state["error"],
            )

        if secret_value:
            print(f"D {secret_value}", flush=True)
        return "OK"

    def handle_pinentry_session(self) -> None:
        """Handle a pinentry session."""
        print("OK", flush=True)

        state = {
            "title": "",
            "prompt": "",
            "desc": "",
            "error": "",
        }

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


def show_help() -> None:
    """Display help message."""
    backend_info = {
        "Darwin": "macOS Keychain",
        "Linux": "Secret Service (KDE Wallet, GNOME Keyring)",
    }
    current_backend = backend_info.get(platform.system(), "system secure storage")

    print(
        f"""Usage: rbw-pinentry [options]

Use this script as pinentry to store rbw master password in secure storage.
Current platform: {platform.system()} using {current_backend}

Options:
  -h, --help, help     Display this help message
  -c, --clear, clear   Clear the stored master password from secure storage

Setup:
  Configure rbw: rbw config set pinentry rbw-pinentry

The password is stored securely in your system's secure storage:
- macOS: Login Keychain
- Linux: Secret Service API (KDE Wallet, GNOME Keyring, etc.)

How it works:
- First time: Prompts for password and caches it in secure storage
- Subsequent uses: Returns cached password from secure storage
- If cached password fails: Automatically clears it and re-prompts
- If manually entered password fails: Re-prompts without clearing cache

Cache state is stored in XDG_CACHE_HOME/rbw-pinentry/ with secure permissions."""
    )


def main() -> None:
    """Main entry point."""
    if len(sys.argv) > 1:
        arg = sys.argv[1]
        if arg in ["-h", "--help", "help"]:
            show_help()
            sys.exit(0)
        if arg in ["-c", "--clear", "clear"]:
            pinentry = PinentryKeychain()
            if pinentry.backend.delete_password(
                pinentry.service_name, pinentry.rbw_profile
            ):
                print(f"Cleared password for profile: {pinentry.rbw_profile}")
            else:
                print(f"No password found for profile: {pinentry.rbw_profile}")
            pinentry.clear_cache_state()
            sys.exit(0)

    # Run as pinentry
    pinentry = PinentryKeychain()
    pinentry.handle_pinentry_session()


if __name__ == "__main__":
    main()
