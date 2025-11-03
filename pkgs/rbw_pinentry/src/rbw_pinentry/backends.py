import logging
import platform
import subprocess
from abc import ABC, abstractmethod
from collections.abc import Callable
from types import TracebackType
from typing import TYPE_CHECKING, ParamSpec, TypeVar

if TYPE_CHECKING:
    import secretstorage

logger = logging.getLogger(__name__)

P = ParamSpec("P")
T = TypeVar("T")


class SecretBackendError(Exception):
    """Exception for secret backend operations."""


class SecretBackend(ABC):
    """Abstract base class for secure password storage backends."""

    def __enter__(self) -> "SecretBackend":
        """Enter context manager (default no-op)."""
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        """Exit context manager (default no-op for backends without persistent connections)."""
        _ = (exc_type, exc_val, exc_tb)  # Unused by default implementation

    @abstractmethod
    def get_password(
        self, service_name: str, account: str
    ) -> str | None:  # pragma: no cover - interface
        pass

    @abstractmethod
    def store_password(
        self, service_name: str, account: str, password: str
    ) -> bool:  # pragma: no cover - interface
        pass

    @abstractmethod
    def delete_password(
        self, service_name: str, account: str
    ) -> bool:  # pragma: no cover - interface
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
        self._connection = None
        self._collection: secretstorage.Collection | None = None

    def _ensure_connection(self) -> "secretstorage.Collection":
        """Lazily establish DBus connection and get collection on first use."""
        if self._collection is None:
            self._connection = self._secretstorage.dbus_init()
            self._collection = self._secretstorage.get_default_collection(
                self._connection
            )
        return self._collection

    def __enter__(self) -> "LinuxSecretServiceBackend":
        """Enter context manager (connection established lazily on first use)."""
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        """Exit context manager, closing DBus connection if it was opened."""
        if self._connection is not None:
            self._connection.close()
            self._connection = None
        self._collection = None

    def _with_unlock_retry(
        self,
        lockable: "secretstorage.Collection | secretstorage.Item",
        func: Callable[[], T],
    ) -> T:
        """Execute a function, unlocking the collection/item if needed on LockedException.

        Args:
            lockable: The secretstorage collection or item to operate on
            func: The function to execute

        Returns:
            The result of func()

        Raises:
            LockedException: If lockable remains locked after unlock attempt
            OSError, RuntimeError: For other operation failures
        """
        try:
            return func()
        except self._secretstorage.exceptions.LockedException:
            logger.debug("Locked, attempting unlock")
            lockable.unlock()
            if lockable.is_locked():
                raise
            return func()

    def get_password(self, service_name: str, account: str) -> str | None:
        try:
            collection = self._ensure_connection()
            items = collection.search_items(
                {"service": service_name, "account": account}
            )
            for item in items:

                def _get_secret(item: "secretstorage.Item" = item) -> str | None:
                    secret = item.get_secret()
                    if isinstance(secret, bytes):
                        return secret.decode("utf-8")
                    return str(secret)

                return self._with_unlock_retry(item, _get_secret)
        except (OSError, RuntimeError) as e:
            logger.warning("Failed to read from Secret Service: %s", e)
        except self._secretstorage.exceptions.LockedException as e:
            logger.warning("Secret Service is locked: %s", e)
        return None

    def store_password(self, service_name: str, account: str, password: str) -> bool:
        try:
            collection = self._ensure_connection()
            items = collection.search_items(
                {"service": service_name, "account": account}
            )
            for item in items:
                # Unlock item before deleting
                self._with_unlock_retry(item, item.delete)

            # Unlock collection before creating
            def _create() -> None:
                collection.create_item(
                    f"rbw master password for {account}",
                    {"service": service_name, "account": account},
                    password.encode("utf-8"),
                    replace=True,
                )

            self._with_unlock_retry(collection, _create)
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
            collection = self._ensure_connection()
            items = collection.search_items(
                {"service": service_name, "account": account}
            )
            for item in items:
                # Unlock item before deleting
                self._with_unlock_retry(item, item.delete)
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
