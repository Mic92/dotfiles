import platform
import subprocess
import logging
from abc import ABC, abstractmethod

logger = logging.getLogger(__name__)


class SecretBackendError(Exception):
    """Exception for secret backend operations."""


class SecretBackend(ABC):
    """Abstract base class for secure password storage backends."""

    @abstractmethod
    def get_password(self, service_name: str, account: str) -> str | None:  # pragma: no cover - interface
        pass

    @abstractmethod
    def store_password(self, service_name: str, account: str, password: str) -> bool:  # pragma: no cover - interface
        pass

    @abstractmethod
    def delete_password(self, service_name: str, account: str) -> bool:  # pragma: no cover - interface
        pass


class MacOSKeychainBackend(SecretBackend):
    """macOS Keychain backend using the security command."""

    def get_password(self, service_name: str, account: str) -> str | None:
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
        try:
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
                    "-U",
                    "-T",
                    "",
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
        try:
            import secretstorage  # noqa: PLC0415

            self._secretstorage = secretstorage
        except ImportError as e:
            msg = "secretstorage module not available. Install python3-secretstorage."
            raise SecretBackendError(msg) from e

    def get_password(self, service_name: str, account: str) -> str | None:
        try:
            with self._secretstorage.dbus_init() as connection:
                collection = self._secretstorage.get_default_collection(connection)
                items = collection.search_items({"service": service_name, "account": account})
                for item in items:
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
        try:
            with self._secretstorage.dbus_init() as connection:
                collection = self._secretstorage.get_default_collection(connection)
                items = collection.search_items({"service": service_name, "account": account})
                for item in items:
                    item.delete()
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
        try:
            with self._secretstorage.dbus_init() as connection:
                collection = self._secretstorage.get_default_collection(connection)
                items = collection.search_items({"service": service_name, "account": account})
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
    system = platform.system()
    if system == "Darwin":
        return MacOSKeychainBackend()
    return LinuxSecretServiceBackend()
