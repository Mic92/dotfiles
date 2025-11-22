"""Matrix Calendar Bot - Encryption bridge for n8n webhooks.

Handles encrypted Matrix messages and forwards commands to n8n using mautrix-python.
"""

import argparse
import asyncio
import json
import logging
import os
from dataclasses import dataclass
from pathlib import Path
from typing import cast

import aiohttp
from mautrix.client import Client
from mautrix.client.state_store.asyncpg import PgStateStore
from mautrix.crypto import OlmMachine, PgCryptoStateStore, PgCryptoStore
from mautrix.types import (
    EventType,
    MessageEvent,
    MessageType,
    RoomID,
    TextMessageEventContent,
)
from mautrix.util.async_db import Database

logger = logging.getLogger(__name__)


@dataclass
class BotConfig:
    """Configuration for the Calendar Bot."""

    homeserver: str
    username: str
    webhook_url: str
    password: str | None = None
    auth_token: str | None = None
    store_path: str | None = None
    device_name: str = "calendar-bot"
    database_url: str = "postgresql:///calendar_bot"
    recovery_key: str | None = None


class CalendarBotBridge:
    def __init__(self, config: BotConfig) -> None:
        self.config = config

        # Set up persistent storage for state and encryption keys
        if config.store_path is None:
            store_path = Path.home() / ".local/share/calendar-bot"
        else:
            store_path = Path(config.store_path)
        self.store_path = store_path
        self.store_path.mkdir(parents=True, exist_ok=True)

        # Client, crypto and state stores will be initialized in async context
        self.client: Client | None = None
        self.crypto_store: PgCryptoStore | None = None
        self.state_store: PgCryptoStateStore | None = None
        self.db: Database | None = None
        self.session_file = self.store_path / "session.json"

    async def init_client(self) -> None:
        """Initialize the Matrix client in async context."""
        # Try to restore session from file
        access_token = None
        device_id = None
        if self.session_file.exists():
            try:
                with self.session_file.open() as f:
                    session_data = json.load(f)
                access_token = session_data.get("access_token")
                device_id = session_data.get("device_id")
                logger.info("Restored session from %s", self.session_file)
            except Exception:
                logger.exception("Failed to restore session")

        # Initialize Matrix client with event loop available
        self.client = Client(
            base_url=self.config.homeserver,
            mxid=self.config.username,
            device_id=device_id or "",
            log=logger,
        )

        # Restore access token if we have one
        if access_token:
            self.client.api.token = access_token
            logger.info("Restored access token")

        # Register message handler
        self.client.add_event_handler(EventType.ROOM_MESSAGE, self.handle_message)

    async def init_crypto(self, db_url: str) -> None:
        """Initialize end-to-end encryption.

        Args:
            db_url: PostgreSQL database URL for crypto store
        """
        assert self.client is not None

        # Create single database connection shared by both stores
        # Use crypto store's upgrade table which includes all crypto schemas
        self.db = Database.create(
            db_url,
            upgrade_table=PgCryptoStore.upgrade_table,
            log=logger,
        )
        await self.db.start()

        # Also run state store upgrades on the same database
        await PgStateStore.upgrade_table.upgrade(self.db)

        # Set up crypto store with PostgreSQL
        self.crypto_store = PgCryptoStore(
            account_id=self.client.mxid,
            pickle_key="calendar.bot.default",
            db=self.db,
        )

        # Set up state store using the same database connection
        # PgCryptoStateStore wraps PgStateStore and shares the database
        self.state_store = PgCryptoStateStore(self.db)

        await self.crypto_store.open()
        await self.state_store.open()

        # Set state store on client
        self.client.state_store = self.state_store

        # Create and configure OlmMachine
        crypto = OlmMachine(self.client, self.crypto_store, self.state_store)
        await crypto.load()

        # Set crypto on client to enable encryption
        self.client.crypto = crypto

        # Check if device keys are actually on the server
        assert crypto.account is not None
        if crypto.account.shared:
            logger.info("Checking if device keys exist on server...")
            resp = await self.client.query_keys(
                {self.client.mxid: [self.client.device_id]}
            )
            device_on_server = (
                self.client.mxid in resp.device_keys
                and self.client.device_id in resp.device_keys.get(self.client.mxid, {})
            )

            if not device_on_server:
                logger.warning(
                    "Account marked as shared but device not found on server, re-uploading keys..."
                )
                crypto.account.shared = False
                await crypto.share_keys()

        logger.info("Encryption initialized")

    async def _verify_with_recovery_key(self) -> None:
        """Verify device using Element recovery key."""
        if not self.config.recovery_key or not self.client or not self.client.crypto:
            return

        try:
            logger.info("Verifying device with recovery key...")
            await self.client.crypto.verify_with_recovery_key(self.config.recovery_key)
            logger.info("Successfully verified device with recovery key")
        except Exception:
            logger.exception("Failed to verify with recovery key")

    async def login(self) -> None:
        """Handle login with password."""
        # Initialize client first (requires event loop)
        await self.init_client()

        assert self.client is not None

        # Only login if we don't have a valid session
        if not self.client.api.token:
            if not self.config.password:
                msg = "Password required for initial login"
                raise ValueError(msg)

            logger.info("Logging in as %s", self.config.username)
            resp = await self.client.login(
                password=self.config.password,
                device_name=self.config.device_name,
            )

            logger.info("Login successful, device_id: %s", self.client.device_id)

            # Save session for future use
            session_data = {
                "access_token": resp.access_token,
                "device_id": resp.device_id,
                "user_id": resp.user_id,
            }
            with self.session_file.open("w") as f:
                json.dump(session_data, f)
            logger.info("Saved session to %s", self.session_file)
        else:
            logger.info("Using existing session, device_id: %s", self.client.device_id)

        # Initialize crypto after successful login
        await self.init_crypto(self.config.database_url)

    async def handle_message(self, evt: MessageEvent) -> None:
        """Handle incoming Matrix messages and forward commands to n8n."""
        assert self.client is not None

        # Ignore our own messages
        if evt.sender == self.client.mxid:
            return

        # Only handle text messages
        if evt.content.msgtype != MessageType.TEXT:
            return

        content = cast("TextMessageEventContent", evt.content)
        text = content.body.strip()

        # Only process commands starting with !
        if not text.startswith("!"):
            return

        room_id = evt.room_id
        sender = evt.sender

        logger.info("Received command: %s from %s in %s", text, sender, room_id)

        # Forward to n8n webhook
        try:
            async with aiohttp.ClientSession() as session:
                payload = {
                    "room_id": str(room_id),
                    "sender": str(sender),
                    "body": {"message": text},
                }

                headers = {}
                if self.config.auth_token:
                    headers["Authorization"] = f"Bearer {self.config.auth_token}"

                async with session.post(
                    self.config.webhook_url,
                    json=payload,
                    headers=headers,
                    timeout=aiohttp.ClientTimeout(total=30),
                ) as resp:
                    if resp.status == 200:
                        try:
                            result = await resp.json()
                            response_text = (
                                result.get("message", "Command processed")
                                if result
                                else "Command processed"
                            )
                        except (aiohttp.ContentTypeError, ValueError):
                            response_text = "Command processed"
                        logger.info("Webhook response: %s", response_text)
                        await self.send_message(room_id, response_text)
                    else:
                        error_text = await resp.text()
                        logger.error("Webhook error %d: %s", resp.status, error_text)
                        await self.send_message(
                            room_id, f"❌ Error processing command: HTTP {resp.status}"
                        )

        except (aiohttp.ClientError, TimeoutError):
            logger.exception("Error forwarding to webhook")
            await self.send_message(room_id, "❌ Failed to connect to webhook")

    async def send_message(self, room_id: RoomID, text: str) -> None:
        """Send a text message to a room."""
        assert self.client is not None
        content = TextMessageEventContent(msgtype=MessageType.TEXT, body=text)
        await self.client.send_message_event(room_id, EventType.ROOM_MESSAGE, content)

    async def run_async(self) -> None:
        """Start the bot asynchronously."""
        await self.login()
        assert self.client is not None

        # Verify with recovery key after first sync (when device is fully registered)
        if self.config.recovery_key:
            # Do one sync first to ensure device is registered on server
            logger.info("Performing initial sync before recovery key verification...")
            await self.client.sync()

            # Now verify with recovery key
            await self._verify_with_recovery_key()

        logger.info("Starting sync loop...")
        await self.client.start(filter_data=None)

    def run(self) -> None:
        """Start the bot."""
        logger.info("Starting Calendar Bot Bridge...")
        logger.info("Homeserver: %s", self.config.homeserver)
        logger.info("Username: %s", self.config.username)
        logger.info("Webhook URL: %s", self.config.webhook_url)

        async def _run_with_cleanup() -> None:
            try:
                await self.run_async()
            except KeyboardInterrupt:
                logger.info("Received keyboard interrupt, shutting down")
            finally:
                if self.state_store:
                    await self.state_store.close()
                if self.crypto_store:
                    await self.crypto_store.close()
                if self.db:
                    await self.db.stop()

        asyncio.run(_run_with_cleanup())


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Matrix Calendar Bot Bridge using mautrix-python"
    )
    parser.add_argument(
        "--homeserver",
        default=os.getenv("MATRIX_HOMESERVER", "https://matrix.org"),
        help="Matrix homeserver URL",
    )
    parser.add_argument(
        "--username",
        default=os.getenv("MATRIX_USERNAME"),
        required=not os.getenv("MATRIX_USERNAME"),
        help="Matrix username (or set MATRIX_USERNAME)",
    )
    parser.add_argument(
        "--password-file",
        required=True,
        help="Path to file containing Matrix password",
    )
    parser.add_argument(
        "--webhook-url",
        default=os.getenv("N8N_WEBHOOK_URL"),
        required=not os.getenv("N8N_WEBHOOK_URL"),
        help="n8n webhook URL (or set N8N_WEBHOOK_URL)",
    )
    parser.add_argument(
        "--auth-token-file",
        help="Path to file containing n8n webhook auth token",
    )
    parser.add_argument(
        "--store-path",
        help="Path to store directory for state and encryption keys (default: ~/.local/share/calendar-bot)",
    )
    parser.add_argument(
        "--device-name",
        default="calendar-bot",
        help="Device name for this Matrix session",
    )
    parser.add_argument(
        "--log-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR"],
        help="Logging level",
    )
    parser.add_argument(
        "--database-url",
        default=os.getenv("DATABASE_URL", "postgresql:///calendar_bot"),
        help="PostgreSQL database URL for crypto store (default: postgresql:///calendar_bot)",
    )
    parser.add_argument(
        "--recovery-key-file",
        help="Path to file containing Element recovery key (optional)",
    )

    args = parser.parse_args()

    # Configure logging
    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )

    # Get password from file
    password_path = Path(args.password_file)
    if not password_path.exists():
        parser.error(f"Password file not found: {password_path}")

    with password_path.open() as f:
        password = f.read().strip()

    if not password:
        parser.error(f"Password file is empty: {password_path}")

    # Get auth token from file if provided
    auth_token = None
    if args.auth_token_file:
        with Path(args.auth_token_file).open() as f:
            auth_token = f.read().strip()

    # Get recovery key from file if provided
    recovery_key = None
    if args.recovery_key_file:
        recovery_key_path = Path(args.recovery_key_file)
        if recovery_key_path.exists():
            with recovery_key_path.open() as f:
                recovery_key = f.read().strip()
        else:
            logger.warning("Recovery key file %s not found", recovery_key_path)

    # Create bot configuration
    config = BotConfig(
        homeserver=args.homeserver,
        username=args.username,
        password=password,
        webhook_url=args.webhook_url,
        auth_token=auth_token,
        store_path=args.store_path,
        device_name=args.device_name,
        database_url=args.database_url,
        recovery_key=recovery_key,
    )

    # Create and run bot
    bot = CalendarBotBridge(config)
    bot.run()


if __name__ == "__main__":
    main()
