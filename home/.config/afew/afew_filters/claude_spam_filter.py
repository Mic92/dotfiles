"""Custom afew filter using Claude Code to classify unknown emails as spam."""

import email
import email.policy
import json
import logging
import subprocess
from pathlib import Path
from typing import Any, cast

from afew.FilterRegistry import register_filter  # type: ignore[import-untyped]
from afew.filters.BaseFilter import Filter  # type: ignore[import-untyped]

# Type alias for notmuch message
from notmuch.message import Message  # type: ignore[import-untyped]

from .spam_database import SpamDatabase

logger = logging.getLogger(__name__)


@register_filter
class ClaudeSpamFilter(Filter):  # type: ignore[misc]
    """Use Claude Code to analyze emails from unknown senders for spam detection."""

    message = "Analyzing emails from unknown senders with Claude Code"
    # Default query - will be modified based on maildir_path config
    query = "NOT tag:spam AND NOT tag:ham AND NOT tag:claude-analyzed"
    # Configuration option for restricting to specific maildir path
    maildir_path = None

    def __init__(self, database: Any, **kwargs: Any) -> None:
        super().__init__(database, **kwargs)

        # Build query based on maildir_path configuration
        base_query = "NOT tag:spam AND NOT tag:ham AND NOT tag:claude-analyzed"
        if self.maildir_path:
            # Add path restriction if configured
            path_query = (
                f"(path:{self.maildir_path}/cur/** OR path:{self.maildir_path}/new/**)"
            )
            self.query = f"{path_query} AND {base_query}"
        else:
            self.query = base_query

        # Initialize spam database
        self.spam_db = SpamDatabase()
        self.spam_db.init_database()

        # Initialize khard contacts as None - will be loaded lazily
        self.khard_contacts: set[str] | None = None

    def _get_khard_contacts(self) -> set[str]:
        """Get all email addresses from khard."""
        try:
            result = subprocess.run(
                ["khard", "email", "--parsable", "--remove-first-line"],
                check=False,
                capture_output=True,
                text=True,
                timeout=5,
            )
            if result.returncode == 0:
                contacts = set()
                for line in result.stdout.strip().split("\n"):
                    if line and "\t" in line:
                        # khard email parsable format: email\tname\ttype
                        parts = line.split("\t", 2)  # Split only on first 2 tabs
                        if parts and parts[0]:
                            email = parts[0].strip().lower()
                            # Validate email format
                            if "@" in email and "." in email.split("@")[1]:
                                contacts.add(email)

                logger.info("Loaded %d email addresses from khard", len(contacts))
                return contacts
            logger.warning("khard email command failed: %s", result.stderr)
        except (subprocess.SubprocessError, OSError):
            logger.exception("Error running khard")
        return set()

    def _is_known_sender(self, email_address: str) -> bool:
        """Check if sender is in khard contacts."""
        # Load contacts lazily on first use
        if self.khard_contacts is None:
            self.khard_contacts = self._get_khard_contacts()

        email = self.spam_db.extract_email_address(email_address)
        return email in self.khard_contacts

    def _extract_email_content(self, filename: str) -> tuple[str, list[str]]:
        """Extract body text and attachment list from email file."""
        body = ""
        attachments = []

        try:
            with Path(filename).open("rb") as f:
                # Cast the policy to satisfy mypy - this is a known typing issue
                msg = email.message_from_binary_file(
                    f, policy=cast("Any", email.policy.default)
                )

                # Extract body and attachments
                for part in msg.walk():
                    content_type = part.get_content_type()
                    content_disposition = str(part.get("Content-Disposition", ""))

                    # Check if it's an attachment
                    if "attachment" in content_disposition:
                        attachment_name = part.get_filename()
                        if attachment_name:
                            attachments.append(f"{attachment_name} ({content_type})")

                    # Extract body text
                    elif part.is_multipart():
                        continue
                    elif content_type == "text/plain":
                        try:
                            payload = part.get_payload(decode=True)
                            if isinstance(payload, bytes):
                                body = payload.decode("utf-8", errors="ignore")[
                                    :1000
                                ]  # First 1000 chars
                                break
                        except (AttributeError, TypeError, UnicodeDecodeError):
                            # Ignore errors in content extraction
                            pass
                    elif content_type == "text/html" and not body:
                        try:
                            html_payload = part.get_payload(decode=True)
                            if isinstance(html_payload, bytes):
                                html_content = html_payload.decode(
                                    "utf-8", errors="ignore"
                                )
                                # Use w3m to convert HTML to text
                                result = subprocess.run(
                                    ["w3m", "-dump", "-T", "text/html"],
                                    check=False,
                                    input=html_content,
                                    capture_output=True,
                                    text=True,
                                    timeout=5,
                                )
                                if result.returncode == 0:
                                    body = result.stdout[:1000]
                        except (
                            subprocess.SubprocessError,
                            AttributeError,
                            TypeError,
                            UnicodeDecodeError,
                        ):
                            # Ignore HTML conversion errors
                            pass

                # If no body found, indicate content type
                if not body and attachments:
                    body = "[No text content, see attachments]"
                elif not body:
                    body = "[No readable text content]"

        except OSError:
            body = "[Error reading body]"

        return body, attachments

    def _extract_headers(self, message: Message) -> dict[str, str]:
        """Extract all relevant headers from the message."""
        return {
            "subject": message.get_header("Subject") or "",
            "from_addr": message.get_header("From") or "",
            "to_addr": message.get_header("To") or "",
            "date": message.get_header("Date") or "",
            "reply_to": message.get_header("Reply-To") or "",
            "return_path": message.get_header("Return-Path") or "",
            "received": message.get_header("Received") or "",
            "list_unsubscribe": message.get_header("List-Unsubscribe") or "",
            "x_mailer": message.get_header("X-Mailer") or "",
            "content_type": message.get_header("Content-Type") or "",
            "cc": message.get_header("Cc") or "",
            "bcc": message.get_header("Bcc") or "",
        }

    def _create_claude_prompt(
        self, headers: dict[str, str], body: str, attachments: list[str]
    ) -> str:
        """Create the prompt for Claude analysis."""
        received_first_line = (
            headers["received"].split("\n")[0] if headers["received"] else ""
        )

        return f"""Analyze this email for spam. Respond with exactly 3 lines:
SPAM: true or false
CONFIDENCE: number from 0 to 100
REASON: brief explanation

Email details:
From: {headers["from_addr"]}
To: {headers["to_addr"]}
Subject: {headers["subject"]}
Date: {headers["date"]}
Reply-To: {headers["reply_to"]}
Return-Path: {headers["return_path"]}
List-Unsubscribe: {headers["list_unsubscribe"]}
X-Mailer: {headers["x_mailer"]}
Content-Type: {headers["content_type"]}
CC: {headers["cc"]}
Received (first line): {received_first_line}
Attachments: {", ".join(attachments) if attachments else "None"}
Body preview: {body}

Consider: suspicious patterns, scam indicators, phishing attempts, unsolicited \
commercial content, mismatch between From/Reply-To/Return-Path, bulk mailing \
indicators, suspicious attachments, etc."""

    def _analyze_with_claude(self, message: Message) -> dict[str, Any] | None:
        """Use Claude Code to analyze if the email is spam."""
        # Extract email details
        headers = self._extract_headers(message)

        # Get body content and attachments
        filename = message.get_filename()
        body, attachments = self._extract_email_content(filename)

        # Clean up attachment names to avoid exposing test paths
        cleaned_attachments = []
        for attachment in attachments:
            # Remove any path components from attachment names
            if "/" in attachment:
                cleaned_attachment = attachment.split("/")[-1]
            else:
                cleaned_attachment = attachment
            cleaned_attachments.append(cleaned_attachment)

        # Create prompt for Claude
        prompt = self._create_claude_prompt(headers, body, cleaned_attachments)

        # Log prompt for debugging (first 500 chars)
        logger.debug("Claude prompt preview: %s...", prompt[:500])

        try:
            # Call Claude Code with prompt via stdin
            result = subprocess.run(
                ["claude", "-c", prompt],
                check=False,
                capture_output=True,
                text=True,
                timeout=100,
            )

            if result.returncode == 0:
                # Parse Claude's response
                response = result.stdout.strip()
                logger.info(
                    "Claude response: %s", response[:200]
                )  # Log first 200 chars

                # Parse string format: SPAM: true/false, CONFIDENCE: number, REASON: text
                lines = response.split("\n")
                result_dict: dict[str, Any] = {}

                for line in lines:
                    stripped_line = line.strip()
                    if stripped_line.startswith("SPAM:"):
                        spam_value = stripped_line.split(":", 1)[1].strip().lower()
                        result_dict["is_spam"] = spam_value == "true"
                    elif stripped_line.startswith("CONFIDENCE:"):
                        try:
                            result_dict["confidence"] = int(
                                stripped_line.split(":", 1)[1].strip()
                            )
                        except ValueError:
                            result_dict["confidence"] = 50
                    elif stripped_line.startswith("REASON:"):
                        result_dict["reason"] = stripped_line.split(":", 1)[1].strip()

                if "is_spam" in result_dict and "confidence" in result_dict:
                    return result_dict
                logger.error("Could not parse Claude response format: %s", response)
            else:
                logger.error("Claude Code error: %s", result.stderr)
                logger.error("Claude Code stdout: %s", result.stdout)
                logger.error("Claude Code returncode: %s", result.returncode)

        except subprocess.TimeoutExpired:
            logger.warning("Claude Code timed out")
        except json.JSONDecodeError:
            logger.exception("Failed to parse Claude response")
        except OSError:
            logger.exception("Error calling Claude Code")

        return None

    def handle_message(self, message: Message) -> None:
        """Process each message."""
        from_addr = message.get_header("From") or ""
        email = self.spam_db.extract_email_address(from_addr)

        # Skip if sender is in khard contacts
        if self._is_known_sender(from_addr):
            logger.debug("Skipping known sender from khard: %s", from_addr)
            return

        # Check spam score in database
        spam_score = self.spam_db.get_spam_score(from_addr)

        # Thresholds for automatic classification
        spam_threshold = 2.0  # Cumulative score above this = spam
        ham_threshold = -2.0  # Cumulative score below this = ham

        if spam_score is not None:
            if spam_score >= spam_threshold:
                # Automatically mark as spam based on history
                logger.info(
                    "Auto-marking as spam based on score %.2f: %s",
                    spam_score,
                    from_addr,
                )
                self.add_tags(message, "spam", "spam-score-high")
                return
            if spam_score <= ham_threshold:
                # Automatically mark as ham based on history
                logger.info(
                    "Auto-marking as ham based on score %.2f: %s", spam_score, from_addr
                )
                self.add_tags(message, "ham", "ham-score-high")
                return

        # For new or uncertain senders, use Claude
        message_id = message.get_message_id()

        # Analyze with Claude
        analysis = self._analyze_with_claude(message)

        if analysis:
            # Log the analysis results
            logger.info(
                "Analysis for %s: is_spam=%s, confidence=%s, reason=%s",
                email,
                analysis.get("is_spam"),
                analysis.get("confidence"),
                analysis.get("reason"),
            )

        # Apply tags based on analysis
        if not analysis:
            logger.warning("No analysis available for message %s", message_id)
            return

        logger.info("Applying tags based on analysis for %s", from_addr)
        self.add_tags(message, "claude-analyzed")

        is_spam = analysis.get("is_spam", False)
        confidence = analysis.get("confidence", 50)

        logger.info("Analysis result: is_spam=%s, confidence=%s", is_spam, confidence)

        # Update spam score in database
        self.spam_db.update_spam_score(from_addr, is_spam, confidence)

        if is_spam:
            logger.info("Adding spam tags for message from %s", from_addr)
            self.add_tags(message, "spam", "claude-spam")
            # Add confidence level tag
            high_confidence = 90
            medium_confidence = 70
            if confidence >= high_confidence:
                self.add_tags(message, "spam-high-confidence")
            elif confidence >= medium_confidence:
                self.add_tags(message, "spam-medium-confidence")
            else:
                self.add_tags(message, "spam-low-confidence")
        else:
            logger.info("Adding ham tags for message from %s", from_addr)
            self.add_tags(message, "ham", "claude-ham")
