#!/usr/bin/env python3
"""Notify Janet (opencrow) about a starred/flagged email.

Called by dovecot's imapsieve via report-flagged.sieve.
Receives the full email on stdin; argv[1] = imap user, argv[2] = mailbox name.
Delivers a copy to Janet's local Maildir and writes a one-line summary to
opencrow's trigger pipe.
"""

import email
import email.header
import email.policy
import html
import mailbox
import os
import pathlib
import re
import stat
import sys

PIPE = "/var/lib/opencrow/sessions/trigger.pipe"
ALLOWED_USER = "joerg@thalheim.io"
JANET_MAILDIR = "/var/vmail/thalheim.io/janet/Maildir"
# Path as seen inside the opencrow container (bind-mounted read-only).
CONTAINER_MAILDIR = "/var/mail/flagged"


def decode_header(value: str) -> str:
    """Decode RFC 2047 encoded header into a plain string."""
    if not value:
        return ""
    parts = email.header.decode_header(value)
    decoded: list[str] = []
    for fragment, charset in parts:
        if isinstance(fragment, bytes):
            decoded.append(fragment.decode(charset or "utf-8", errors="replace"))
        else:
            decoded.append(fragment)
    # Join without adding spaces — the raw header already contains any
    # intended whitespace between encoded-words per RFC 2047.
    return "".join(decoded).strip()


def _decode_part(part: email.message.Message) -> str | None:
    """Decode a single MIME part's payload to str, or None on failure."""
    payload = part.get_payload(decode=True)
    if not isinstance(payload, bytes):
        return None
    charset = part.get_content_charset() or "utf-8"
    return payload.decode(charset, errors="replace")


def html_to_text(raw_html: str) -> str:
    """Very rough HTML-to-text: strip script/style blocks and tags."""
    # Remove <script> and <style> blocks entirely
    text = re.sub(
        r"<(script|style)[^>]*>.*?</\1>", " ", raw_html, flags=re.DOTALL | re.IGNORECASE
    )
    text = re.sub(r"<[^>]+>", " ", text)
    return html.unescape(text)


def _collect_text_parts(msg: email.message.Message) -> tuple[list[str], list[str]]:
    """Walk a message and return (plain_parts, html_parts) text content."""
    plain_parts: list[str] = []
    html_parts: list[str] = []

    parts = msg.walk() if msg.is_multipart() else [msg]
    for part in parts:
        ct = part.get_content_type()
        text = _decode_part(part)
        if not text:
            continue
        if ct == "text/plain":
            plain_parts.append(text)
        elif ct == "text/html":
            html_parts.append(text)

    return plain_parts, html_parts


def _truncate_snippet(body: str, max_lines: int) -> str:
    """Take the first non-empty lines and truncate to 500 chars."""
    lines = [line.strip() for line in body.splitlines() if line.strip()]
    snippet = " ".join(lines[:max_lines])
    snippet = " ".join(snippet.split())
    if len(snippet) > 500:
        snippet = snippet[:500] + "…"
    return snippet


def get_body_snippet(msg: email.message.Message, max_lines: int = 10) -> str:
    """Extract a plain-text snippet from the email body."""
    plain_parts, html_parts = _collect_text_parts(msg)

    if plain_parts:
        body = plain_parts[0]
    elif html_parts:
        body = html_to_text(html_parts[0])
    else:
        return ""

    return _truncate_snippet(body, max_lines)


def build_trigger_line(
    msg: email.message.Message,
    folder: str,
    maildir_key: str,
    container_maildir: str = CONTAINER_MAILDIR,
) -> str:
    """Build the one-line trigger summary for opencrow."""
    subject = decode_header(msg.get("Subject", ""))
    from_addr = decode_header(msg.get("From", ""))
    date = msg.get("Date", "")
    snippet = get_body_snippet(msg)

    # Maildir delivers to new/, so Janet can read the full email at this path.
    mail_path = f"{container_maildir}/new/{maildir_key}"

    trigger = (
        f"Joerg starred an email — "
        f"From: {from_addr} | "
        f"Subject: {subject} | "
        f"Date: {date} | "
        f"Folder: {folder} | "
        f"File: {mail_path} | "
        f"Snippet: {snippet}"
    )
    return trigger.replace("\n", " ").replace("\r", "")


def _sanitize_message_id(message_id: str) -> str:
    """Turn a Message-ID into a safe filename (strip angle brackets, replace /)."""
    return message_id.strip().strip("<>").replace("/", "_")


def deliver_to_maildir(msg: email.message.Message, maildir_path: str) -> str | None:
    """Deliver a copy of the email into a Maildir's new/ directory.

    Uses the Message-ID as the filename so re-flagging the same email
    is a no-op.  Returns the key (filename) on first delivery, or None
    if the message was already present.
    """
    message_id = msg.get("Message-ID", "")
    if not message_id:
        # No Message-ID — fall back to Maildir's own key generation.
        old_umask = os.umask(0o007)
        try:
            mdir = mailbox.Maildir(maildir_path, create=True)
            try:
                key = mdir.add(mailbox.MaildirMessage(msg))
            finally:
                mdir.close()
        finally:
            os.umask(old_umask)
        return key

    key = _sanitize_message_id(message_id)
    new_dir = pathlib.Path(maildir_path) / "new"
    new_dir.mkdir(parents=True, exist_ok=True)
    dest = new_dir / key

    if dest.exists():
        return None

    old_umask = os.umask(0o007)
    try:
        dest.write_bytes(msg.as_bytes())
    finally:
        os.umask(old_umask)
    return key


def write_trigger_pipe(trigger: str, pipe_path: str) -> None:
    """Write trigger line to a FIFO, failing silently if unavailable."""
    try:
        p = pathlib.Path(pipe_path)
        if stat.S_ISFIFO(p.stat().st_mode):
            fd = os.open(pipe_path, os.O_WRONLY | os.O_NONBLOCK)
            try:
                os.write(fd, (trigger + "\n").encode())
            finally:
                os.close(fd)
    except OSError:
        # Janet not running, pipe missing, or pipe full — fail silently
        pass


def process_flagged_email(
    raw: bytes,
    folder: str,
    maildir_path: str,
    pipe_path: str,
    container_maildir: str = CONTAINER_MAILDIR,
) -> str | None:
    """Process a flagged email: deliver to Maildir and return trigger line.

    Returns the trigger line (also written to the pipe if available),
    or None if the message was already delivered (duplicate).
    """
    msg = email.message_from_bytes(raw, policy=email.policy.compat32)

    key = deliver_to_maildir(msg, maildir_path)
    if key is None:
        return None

    trigger = build_trigger_line(msg, folder, key, container_maildir)
    write_trigger_pipe(trigger, pipe_path)

    return trigger


def main() -> None:
    if len(sys.argv) < 3:
        print("Usage: notify-flagged.py <user> <mailbox>", file=sys.stderr)
        sys.exit(1)

    user = sys.argv[1]
    folder = sys.argv[2]

    if user != ALLOWED_USER:
        sys.exit(0)

    raw = sys.stdin.buffer.read()
    process_flagged_email(raw, folder, JANET_MAILDIR, PIPE)


if __name__ == "__main__":
    main()
