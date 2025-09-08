"""
rbw pinentry wrapper for macOS Keychain.

Stores and retrieves master passwords from macOS Keychain.
Detects password validation failures through SETERROR messages.
"""

import logging
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

# Set up logging to stderr so it doesn't interfere with pinentry protocol
logging.basicConfig(
    level=logging.WARNING,
    format="%(message)s",
    stream=sys.stderr,
)
logger = logging.getLogger(__name__)


class KeychainError(Exception):
    """Exception for keychain operations."""


class PinentryKeychain:
    """Pinentry wrapper that caches passwords in macOS Keychain."""

    def __init__(self) -> None:
        """Initialize the pinentry keychain wrapper."""
        self.rbw_profile = os.environ.get("RBW_PROFILE", "rbw")
        self.service_name = "rbw-master-password"
        self.pinentry_cmd = self._find_pinentry()
        # Use XDG_CACHE_HOME or fallback to ~/.cache
        cache_dir = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache"))
        cache_dir = cache_dir / "rbw-pinentry-keychain"
        cache_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
        self.cache_state_file = cache_dir / f"{self.rbw_profile}-cache-state"

    def _find_pinentry(self) -> str:
        """Find the actual pinentry command to use."""
        # Just use whatever pinentry-mac or pinentry is in PATH
        pinentry = shutil.which("pinentry-mac") or shutil.which("pinentry")
        if not pinentry:
            msg = "Could not find pinentry-mac or pinentry in PATH"
            raise RuntimeError(msg)
        return pinentry

    def get_password_from_keychain(self) -> str | None:
        """Retrieve password from macOS Keychain."""
        try:
            result = subprocess.run(
                [
                    "security",
                    "find-generic-password",
                    "-a",
                    self.rbw_profile,
                    "-s",
                    self.service_name,
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

    def store_password_in_keychain(self, password: str) -> bool:
        """Store password in macOS Keychain."""
        try:
            # Use -U to update if exists
            subprocess.run(
                [
                    "security",
                    "add-generic-password",
                    "-a",
                    self.rbw_profile,
                    "-s",
                    self.service_name,
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

    def delete_password_from_keychain(self) -> bool:
        """Delete password from macOS Keychain."""
        try:
            subprocess.run(
                [
                    "security",
                    "delete-generic-password",
                    "-a",
                    self.rbw_profile,
                    "-s",
                    self.service_name,
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

    def call_real_pinentry(self, commands: list[str]) -> str | None:
        """Call the real pinentry program and get the password."""
        try:
            # Prepare the command string
            cmd_str = "\n".join(commands) + "\n"

            # Call the real pinentry
            result = subprocess.run(
                [self.pinentry_cmd, *sys.argv[1:]],
                input=cmd_str,
                capture_output=True,
                text=True,
                check=False,
            )

            # Parse the output to find the password
            for line in result.stdout.splitlines():
                if line.startswith("D "):
                    return line[2:]  # Return everything after "D "

        except OSError as e:
            logger.warning("Failed to call real pinentry: %s", e)
        return None

    def _handle_master_password(
        self,
        error: str,
    ) -> str | None:
        """Handle master password request with caching logic."""
        # Check if we have an error
        if error:
            # Check if this error is from a cached password
            if self.was_cache_used_recently():
                # The cached password was wrong, clear it
                self.delete_password_from_keychain()
                self.clear_cache_state()
                # Force re-prompt
                cached_password = None
            else:
                # User entered wrong password manually, don't clear cache
                # but do re-prompt
                cached_password = None
        else:
            # No error, try to get from keychain
            cached_password = self.get_password_from_keychain()

        if cached_password:
            # Mark that we're using cached password
            self.mark_cache_used()
            return cached_password

        # Need to prompt for password
        self.clear_cache_state()  # Not using cache

        commands = ["SETTITLE rbw", "SETPROMPT Master Password"]

        if error:
            commands.append(f"SETERROR {error}")
            commands.append(
                f"SETDESC Authentication failed. Please enter the master password for '{self.rbw_profile}'"
            )
        else:
            commands.append(
                f"SETDESC Please enter the master password for '{self.rbw_profile}' (will be cached in keychain)"
            )

        commands.append("GETPIN")

        secret_value = self.call_real_pinentry(commands)

        # Store in keychain if we got a password and there was no error
        # We can't validate it here (would cause recursion), but if it's wrong,
        # the next call will have SETERROR set and we'll clear it
        if secret_value and not error:
            self.store_password_in_keychain(secret_value)

        return secret_value

    def _handle_other_password(
        self,
        title: str,
        prompt: str,
        desc: str,
        error: str,
    ) -> str | None:
        """Handle non-master password prompts."""
        commands = []
        if title:
            commands.append(f"SETTITLE {title}")
        if prompt:
            commands.append(f"SETPROMPT {prompt}")
        if desc:
            commands.append(f"SETDESC {desc}")
        if error:
            commands.append(f"SETERROR {error}")
        commands.append("GETPIN")

        return self.call_real_pinentry(commands)

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
    print(
        """Usage: rbw-pinentry-keychain [options]

Use this script as pinentry to store rbw master password in macOS Keychain.

Options:
  -h, --help, help     Display this help message
  -c, --clear, clear   Clear the stored master password from the keychain

Setup:
  Configure rbw: rbw config set pinentry rbw-pinentry-keychain

The password is stored securely in your macOS login keychain.

How it works:
- First time: Prompts for password and caches it in keychain
- Subsequent uses: Returns cached password from keychain
- If cached password fails: Automatically clears it and re-prompts
- If manually entered password fails: Re-prompts without clearing cache

Cache state is stored in XDG_CACHE_HOME/rbw-pinentry-keychain/ with secure permissions."""
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
            if pinentry.delete_password_from_keychain():
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
