"""Matrix Calendar Bot - Encryption bridge for n8n webhooks.

Handles encrypted Matrix messages and forwards commands to n8n.
"""

import argparse
import os
from pathlib import Path

import aiohttp
import simplematrixbotlib as botlib
from nio import MatrixRoom, RoomMessageText  # type: ignore[import-untyped]


class CalendarBotBridge:
    def __init__(
        self,
        homeserver: str,
        username: str,
        password: str,
        webhook_url: str,
        auth_token: str | None = None,
    ) -> None:
        self.webhook_url = webhook_url
        self.auth_token = auth_token

        self.creds = botlib.Creds(homeserver, username, password)  # type: ignore[no-untyped-call]
        self.config = botlib.Config()
        self.config.encryption_enabled = True
        self.config.emoji_verify = True
        self.config.ignore_unverified_devices = True

        self.bot = botlib.Bot(self.creds, self.config)

        # Register message handler
        self.bot.listener.on_message_event(self.handle_message)  # type: ignore[arg-type]

    async def handle_message(self, room: MatrixRoom, message: RoomMessageText) -> None:
        """Handle incoming Matrix messages and forward commands to n8n."""
        # Ignore our own messages
        match = botlib.MessageMatch(room, message, self.bot)
        if match.is_not_from_this_bot() is False:  # type: ignore[no-untyped-call]
            return

        text = message.body.strip()

        # Only process commands starting with !
        if not text.startswith("!"):
            return

        room_id = room.room_id
        sender = message.sender

        # Forward to n8n webhook
        try:
            async with aiohttp.ClientSession() as session:
                payload = {
                    "room_id": room_id,
                    "sender": sender,
                    "body": {"message": text},
                }

                print(
                    f"Forwarding command to webhook: {text} from {sender} in {room_id}"
                )

                headers = {}
                if self.auth_token:
                    headers["Authorization"] = f"Bearer {self.auth_token}"

                async with session.post(
                    self.webhook_url,
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
                        print(f"Webhook response: {response_text}")
                        await self.bot.api.send_text_message(room_id, response_text)
                    else:
                        error_text = await resp.text()
                        error_msg = f"Webhook error {resp.status}: {error_text}"
                        print(error_msg)
                        await self.bot.api.send_text_message(
                            room_id, f"❌ Error processing command: HTTP {resp.status}"
                        )

        except (aiohttp.ClientError, TimeoutError) as e:
            error_msg = f"Error forwarding to webhook: {e}"
            print(error_msg)
            await self.bot.api.send_text_message(
                room_id, f"❌ Failed to connect to webhook: {e}"
            )

    def run(self) -> None:
        """Start the bot."""
        print("Starting Calendar Bot Bridge...")
        print(f"Webhook URL: {self.webhook_url}")
        self.bot.run()


def main() -> None:
    parser = argparse.ArgumentParser(description="Matrix Calendar Bot Bridge")
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
        "--password",
        default=os.getenv("MATRIX_PASSWORD"),
        help="Matrix password (or set MATRIX_PASSWORD)",
    )
    parser.add_argument(
        "--password-file",
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

    args = parser.parse_args()

    # Get password from file or argument
    password = args.password
    if args.password_file:
        with Path(args.password_file).open() as f:
            password = f.read().strip()

    if not password:
        parser.error("Either --password or --password-file must be provided")

    # Get auth token from file if provided
    auth_token = None
    if args.auth_token_file:
        with Path(args.auth_token_file).open() as f:
            auth_token = f.read().strip()

    # Create and run bot
    bot = CalendarBotBridge(
        args.homeserver, args.username, password, args.webhook_url, auth_token
    )
    bot.run()


if __name__ == "__main__":
    main()
