#!/usr/bin/env python3
"""Notify Janet (opencrow) about a starred/flagged email.

Called by dovecot's imapsieve via report-flagged.sieve.
Receives the full email on stdin; argv[1] = imap user, argv[2] = mailbox name.
Delivers a copy to Janet's local Maildir and writes a one-line summary to
opencrow's trigger pipe.  After forwarding, removes the \\Flagged flag from the
source Maildir file so the star disappears in the user's mail client.
"""

import email
import email.header
import email.policy
import html
import logging
import logging.handlers
import mailbox
import os
import pathlib
import re
import stat
import sys
from dataclasses import dataclass

log = logging.getLogger("notify-flagged")

ALLOWED_USER = "joerg@thalheim.io"


@dataclass
class Config:
    """Paths used by the forwarding pipeline."""

    pipe: str = "/var/lib/opencrow/sessions/trigger.pipe"
    janet_maildir: str = "/var/vmail/thalheim.io/janet/Maildir"
    container_maildir: str = "/var/mail/flagged"
    user_maildir: str = "/var/vmail/thalheim.io/joerg/Maildir"


def setup_logging() -> None:
    """Configure logging to syslog with LOG_MAIL facility."""
    handler = logging.handlers.SysLogHandler(
        address="/dev/log", facility=logging.handlers.SysLogHandler.LOG_MAIL
    )
    handler.setFormatter(logging.Formatter("%(name)s: %(message)s"))
    log.addHandler(handler)
    log.setLevel(logging.INFO)


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
    return "".join(decoded).strip()


def html_to_text(raw_html: str) -> str:
    """Very rough HTML-to-text: strip script/style blocks and tags."""
    text = re.sub(
        r"<(script|style)[^>]*>.*?</\1>", " ", raw_html, flags=re.DOTALL | re.IGNORECASE
    )
    text = re.sub(r"<[^>]+>", " ", text)
    return html.unescape(text)


def get_body_snippet(msg: email.message.Message, max_lines: int = 10) -> str:
    """Extract a short plain-text snippet from the email body."""
    plain_parts: list[str] = []
    html_parts: list[str] = []

    for part in msg.walk() if msg.is_multipart() else [msg]:
        payload = part.get_payload(decode=True)
        if not isinstance(payload, bytes):
            continue
        charset = part.get_content_charset() or "utf-8"
        text = payload.decode(charset, errors="replace")
        ct = part.get_content_type()
        if ct == "text/plain":
            plain_parts.append(text)
        elif ct == "text/html":
            html_parts.append(text)

    body = (plain_parts or html_parts or [""])[0]
    if not plain_parts and html_parts:
        body = html_to_text(body)

    lines = [line.strip() for line in body.splitlines() if line.strip()]
    snippet = " ".join(" ".join(lines[:max_lines]).split())
    return snippet[:500] + "…" if len(snippet) > 500 else snippet


def build_trigger_line(
    msg: email.message.Message,
    folder: str,
    maildir_key: str,
    container_maildir: str,
) -> str:
    """Build the one-line trigger summary for opencrow."""
    subject = decode_header(msg.get("Subject", ""))
    from_addr = decode_header(msg.get("From", ""))
    date = msg.get("Date", "")
    snippet = get_body_snippet(msg)
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


def deliver_to_maildir(msg: email.message.Message, maildir_path: str) -> str | None:
    """Deliver a copy of the email into a Maildir's new/ directory.

    Uses the Message-ID as the filename so re-flagging the same email
    is a no-op.  Returns the key (filename) on first delivery, or None
    if the message was already present.
    """
    message_id = msg.get("Message-ID", "")
    if not message_id:
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

    key = message_id.strip().strip("<>").replace("/", "_")
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


# ── Maildir flag helpers ─────────────────────────────────────────────────


def maildir_subdir(mailbox_name: str, base_maildir: str) -> pathlib.Path:
    """INBOX → base Maildir; other folders → .FolderName (dots for nesting)."""
    if mailbox_name.upper() == "INBOX":
        return pathlib.Path(base_maildir)
    return pathlib.Path(base_maildir) / ("." + mailbox_name.replace("/", "."))


def is_flagged(filename: str) -> bool:
    """Check whether a Maildir filename has the Flagged (F) flag."""
    return ":2," in filename and "F" in filename.rsplit(":2,", 1)[1]


def find_source_file(
    message_id: str, mailbox_name: str, base_maildir: str
) -> pathlib.Path | None:
    """Find the Maildir file matching a Message-ID in the given mailbox."""
    if not message_id:
        return None

    maildir = maildir_subdir(mailbox_name, base_maildir)
    for subdir_name in ("cur", "new"):
        subdir = maildir / subdir_name
        if not subdir.is_dir():
            continue
        for entry in subdir.iterdir():
            if not entry.is_file():
                continue
            try:
                with entry.open("rb") as f:
                    for line in f:
                        if line.strip() == b"":
                            break
                        if message_id.encode() in line:
                            return entry
            except OSError:
                continue
    return None


def remove_flagged_flag(filepath: pathlib.Path) -> pathlib.Path | None:
    """Remove the 'F' (Flagged) flag from a Maildir filename by renaming."""
    name = filepath.name
    if ":2," not in name:
        return None
    prefix, flags = name.rsplit(":2,", 1)
    if "F" not in flags:
        return None
    new_path = filepath.parent / f"{prefix}:2,{flags.replace('F', '')}"
    try:
        filepath.rename(new_path)
    except OSError:
        return None
    else:
        return new_path


# ── Trigger pipe ─────────────────────────────────────────────────────────


def write_trigger_pipe(trigger: str, pipe_path: str) -> None:
    """Write trigger line to a FIFO, logging on success or failure."""
    try:
        p = pathlib.Path(pipe_path)
        if not stat.S_ISFIFO(p.stat().st_mode):
            log.warning("%s exists but is not a FIFO", pipe_path)
            return
        fd = os.open(pipe_path, os.O_WRONLY | os.O_NONBLOCK)
        try:
            os.write(fd, (trigger + "\n").encode())
        finally:
            os.close(fd)
        log.info("wrote trigger to %s", pipe_path)
    except OSError as e:
        log.warning("could not write to trigger pipe %s: %s", pipe_path, e)


# ── Main pipeline ───────────────────────────────────────────────────────


def process_flagged_email(
    raw: bytes,
    folder: str,
    cfg: Config,
) -> str | None:
    """Process a flagged email: deliver to Maildir, remove flag, return trigger.

    Only forwards messages that actually carry the \\Flagged flag in their
    Maildir filename.  After a successful forward the flag is removed from
    the source file so the star disappears in the user's mail client.

    Returns the trigger line, or None if duplicate / not flagged.
    """
    msg = email.message_from_bytes(raw, policy=email.policy.compat32)
    subject = decode_header(msg.get("Subject", "(no subject)"))
    message_id = msg.get("Message-ID", "")

    # Locate the source file in the user's Maildir and verify it's flagged.
    source = find_source_file(message_id, folder, cfg.user_maildir)
    if source is not None and not is_flagged(source.name):
        log.info("skipping unflagged message in %s: %s", folder, subject)
        return None
    if source is None:
        log.warning("source file not found for message-id=%s in %s", message_id, folder)

    key = deliver_to_maildir(msg, cfg.janet_maildir)
    if key is None:
        log.info("duplicate, already delivered: %s", subject)
        return None

    trigger = build_trigger_line(msg, folder, key, cfg.container_maildir)
    write_trigger_pipe(trigger, cfg.pipe)

    # Remove the Flagged flag from the source so the star disappears.
    if source is not None:
        new_path = remove_flagged_flag(source)
        if new_path is not None:
            log.info("unflagged %s -> %s", source.name, new_path.name)
        else:
            log.warning("failed to remove Flagged flag from %s", source)

    log.info("forwarded from %s: %s", folder, subject)
    return trigger


def main() -> None:
    setup_logging()

    if len(sys.argv) < 3:
        log.error("Usage: notify-flagged.py <user> <mailbox>")
        sys.exit(1)

    user = sys.argv[1]
    folder = sys.argv[2]

    if user != ALLOWED_USER:
        sys.exit(0)

    raw = sys.stdin.buffer.read()
    process_flagged_email(raw, folder, Config())


if __name__ == "__main__":
    main()
