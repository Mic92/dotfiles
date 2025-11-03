"""Custom afew filter using Claude Code to classify unknown emails as spam."""

import email
import email.policy
import logging
import os
import subprocess
import tempfile
from pathlib import Path
from typing import Any, cast

from afew.FilterRegistry import register_filter  # type: ignore[import-not-found]
from afew.filters.BaseFilter import Filter  # type: ignore[import-not-found]
from notmuch.errors import NullPointerError  # type: ignore[import-not-found]

# Type alias for notmuch message
from notmuch.message import Message  # type: ignore[import-not-found]

from .spam_database import SpamDatabase

logger = logging.getLogger(__name__)


@register_filter
class ClaudeSpamFilter(Filter):  # type: ignore[misc]
    """Use Claude Code to analyze emails from unknown senders for spam detection."""

    message = "Analyzing emails from unknown senders with Claude Code"
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

        # Check if we have too many messages to process
        try:
            # Count messages matching our query using afew's database wrapper
            messages = database.get_messages(self.query)
            self.message_count = len(list(messages))
            self.max_initial_messages = 40  # Limit for initial run
            self.skip_claude = self.message_count > self.max_initial_messages

            if self.skip_claude:
                logger.warning(
                    "Found %d unanalyzed messages (limit: %d). Marking all as analyzed to start fresh.",
                    self.message_count,
                    self.max_initial_messages,
                )
                # Use notmuch to bulk tag all unanalyzed messages
                result = subprocess.run(
                    ["notmuch", "tag", "+claude-analyzed", "--", self.query],
                    check=False,
                    capture_output=True,
                    text=True,
                )
                if result.returncode == 0:
                    logger.info(
                        "Successfully marked %d messages as analyzed",
                        self.message_count,
                    )
                else:
                    logger.error("Failed to bulk tag messages: %s", result.stderr)
        except Exception:
            logger.exception("Error counting messages")
            self.skip_claude = False
            self.message_count = 0

    def _get_khard_contacts(self) -> set[str]:
        """Get all email addresses from khard."""
        try:
            # Set LC_ALL=C to avoid locale issues with khard's sorting
            env = os.environ.copy()
            env["LC_ALL"] = "C"

            result = subprocess.run(
                ["khard", "email", "--parsable", "--remove-first-line"],
                check=False,
                capture_output=True,
                text=True,
                timeout=5,
                env=env,
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
        try:
            with Path(filename).open("rb") as f:
                # Cast the policy to satisfy mypy - this is a known typing issue
                msg = email.message_from_binary_file(
                    f, policy=cast("Any", email.policy.default)
                )
                return self._process_email_parts(msg)
        except OSError:
            return "[Error reading body]", []

    def _process_email_parts(self, msg: Any) -> tuple[str, list[str]]:
        """Process email parts to extract body and attachments."""
        body = ""
        attachments = []

        for part in msg.walk():
            content_type = part.get_content_type()
            content_disposition = str(part.get("Content-Disposition", ""))

            if "attachment" in content_disposition:
                attachment_name = part.get_filename()
                if attachment_name:
                    attachments.append(f"{attachment_name} ({content_type})")
            elif part.is_multipart():
                continue
            elif content_type == "text/plain" and not body:
                body = self._extract_text_content(part)
            elif content_type == "text/html" and not body:
                body = self._extract_html_content(part)

        # Provide default body if none found
        if not body:
            body = (
                "[No text content, see attachments]"
                if attachments
                else "[No readable text content]"
            )

        return body, attachments

    def _extract_text_content(self, part: Any) -> str:
        """Extract plain text content from email part."""
        try:
            payload = part.get_payload(decode=True)
            if isinstance(payload, bytes):
                return payload.decode("utf-8", errors="ignore")[:1000]
        except (AttributeError, TypeError, UnicodeDecodeError):
            pass
        return ""

    def _extract_html_content(self, part: Any) -> str:
        """Extract and convert HTML content to text."""
        try:
            html_payload = part.get_payload(decode=True)
            if isinstance(html_payload, bytes):
                html_content = html_payload.decode("utf-8", errors="ignore")
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
                    return result.stdout[:1000]
        except (
            subprocess.SubprocessError,
            AttributeError,
            TypeError,
            UnicodeDecodeError,
        ):
            pass
        return ""

    def _extract_headers(self, message: Message) -> dict[str, str]:
        """Extract all relevant headers from the message."""
        headers = {}
        header_map = {
            "subject": "Subject",
            "from_addr": "From",
            "to_addr": "To",
            "date": "Date",
            "reply_to": "Reply-To",
            "return_path": "Return-Path",
            "received": "Received",
            "list_unsubscribe": "List-Unsubscribe",
            "x_mailer": "X-Mailer",
            "content_type": "Content-Type",
            "cc": "Cc",
            "bcc": "Bcc",
        }

        for key, header_name in header_map.items():
            try:
                headers[key] = message.get_header(header_name) or ""
            except NullPointerError:
                # Handle NullPointerError when mail file doesn't exist or file descriptors exhausted
                headers[key] = ""

        return headers

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

        # Clean up attachment names
        cleaned_attachments = self._clean_attachment_names(attachments)

        # Create prompt for Claude
        prompt = self._create_claude_prompt(headers, body, cleaned_attachments)

        # Log prompt for debugging (first 500 chars)
        logger.debug("Claude prompt preview: %s...", prompt[:500])

        return self._call_claude_api(prompt)

    def _clean_attachment_names(self, attachments: list[str]) -> list[str]:
        """Remove path components from attachment names."""
        cleaned = []
        for attachment in attachments:
            # Remove any path components from attachment names
            cleaned_name = (
                attachment.split("/")[-1] if "/" in attachment else attachment
            )
            cleaned.append(cleaned_name)
        return cleaned

    def _call_claude_api(self, prompt: str) -> dict[str, Any] | None:
        """Call Claude API and parse response."""

        try:
            # Create a temporary directory to run claude in (no file context)
            with tempfile.TemporaryDirectory() as tmpdir:
                # Call Claude Code with prompt via stdin in print mode with tools disabled
                result = subprocess.run(
                    [
                        "claude",
                        "-p",
                        "--model",
                        "sonnet",
                        "--disallowed-tools",
                        "Bash,Edit,Write,Read,Grep,Glob,Task,WebSearch,WebFetch,TodoWrite,MultiEdit,NotebookEdit,ExitPlanMode,BashOutput,KillBash",
                    ],
                    input=prompt,
                    check=False,
                    capture_output=True,
                    text=True,
                    timeout=100,
                    cwd=tmpdir,  # Run in temp directory to avoid file context
                )

                if result.returncode == 0:
                    return self._parse_claude_response(result.stdout.strip())
                logger.error("Claude Code error: %s", result.stderr)
                logger.error("Claude Code stdout: %s", result.stdout)
                logger.error("Claude Code returncode: %s", result.returncode)

        except subprocess.TimeoutExpired:
            logger.warning("Claude Code timed out")
        except OSError:
            logger.exception("Error calling Claude Code")

        return None

    def _parse_claude_response(self, response: str) -> dict[str, Any] | None:
        """Parse Claude's response into structured data."""
        logger.info("Claude response: %s", response[:200])  # Log first 200 chars

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
        return None

    def handle_message(self, message: Message) -> None:
        """Process each message."""
        # Check if we should skip Claude analysis due to too many messages
        if self.skip_claude:
            # Already bulk tagged in __init__, skip processing
            return

        from_addr = message.get_header("From") or ""

        # Skip if sender is in khard contacts
        if self._is_known_sender(from_addr):
            logger.debug("Skipping known sender from khard: %s", from_addr)
            return

        # Check if we can auto-classify based on history
        if self._auto_classify_by_history(message, from_addr):
            return

        # For new or uncertain senders, use Claude
        self._analyze_and_tag_message(message, from_addr)

    def _auto_classify_by_history(self, message: Message, from_addr: str) -> bool:
        """Try to auto-classify based on spam score history. Returns True if classified."""
        spam_score = self.spam_db.get_spam_score(from_addr)
        if spam_score is None:
            return False

        # Thresholds for automatic classification
        spam_threshold = 2.0  # Cumulative score above this = spam
        ham_threshold = -2.0  # Cumulative score below this = ham

        if spam_score >= spam_threshold:
            logger.info(
                "Auto-marking as spam based on score %.2f: %s", spam_score, from_addr
            )
            self.add_tags(message, "spam", "spam-score-high")
            return True
        if spam_score <= ham_threshold:
            logger.info(
                "Auto-marking as ham based on score %.2f: %s", spam_score, from_addr
            )
            self.add_tags(message, "ham", "ham-score-high")
            return True

        return False

    def _analyze_and_tag_message(self, message: Message, from_addr: str) -> None:
        """Analyze message with Claude and apply appropriate tags."""
        message_id = message.get_message_id()
        analysis = self._analyze_with_claude(message)

        if not analysis:
            logger.warning("No analysis available for message %s", message_id)
            return

        # Log the analysis results
        email = self.spam_db.extract_email_address(from_addr)
        logger.info(
            "Analysis for %s: is_spam=%s, confidence=%s, reason=%s",
            email,
            analysis.get("is_spam"),
            analysis.get("confidence"),
            analysis.get("reason"),
        )

        # Apply tags
        self._apply_analysis_tags(message, from_addr, analysis)

    def _apply_analysis_tags(
        self, message: Message, from_addr: str, analysis: dict[str, Any]
    ) -> None:
        """Apply tags based on Claude's analysis."""
        self.add_tags(message, "claude-analyzed")

        is_spam = analysis.get("is_spam", False)
        confidence = analysis.get("confidence", 50)

        logger.info("Analysis result: is_spam=%s, confidence=%s", is_spam, confidence)

        # Update spam score in database
        self.spam_db.update_spam_score(from_addr, is_spam, confidence)

        if is_spam:
            logger.info("Adding spam tags for message from %s", from_addr)
            self.add_tags(message, "spam", "claude-spam")
            self._add_confidence_tags(message, confidence)
        else:
            logger.info("Adding ham tags for message from %s", from_addr)
            self.add_tags(message, "ham", "claude-ham")

    def _add_confidence_tags(self, message: Message, confidence: int) -> None:
        """Add confidence level tags for spam."""
        high_confidence = 90
        medium_confidence = 70

        if confidence >= high_confidence:
            self.add_tags(message, "spam-high-confidence")
        elif confidence >= medium_confidence:
            self.add_tags(message, "spam-medium-confidence")
        else:
            self.add_tags(message, "spam-low-confidence")
