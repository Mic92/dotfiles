"""Tests for notify-flagged.py — run with: python test_notify_flagged.py"""  # noqa: INP001

import email
import email.policy
import importlib.util
import os
import sys
import tempfile
from pathlib import Path

# Load the script as a module despite the hyphenated filename.
_spec = importlib.util.spec_from_file_location(
    "notify_flagged",
    Path(__file__).parent / "notify-flagged.py",
)
assert _spec is not None
assert _spec.loader is not None
notify_flagged = importlib.util.module_from_spec(_spec)
sys.modules["notify_flagged"] = notify_flagged
_spec.loader.exec_module(notify_flagged)


def _parse(raw: bytes) -> email.message.Message:
    return email.message_from_bytes(raw, policy=email.policy.compat32)


# ── Realistic email fixtures ────────────────────────────────────────────

RFC2047_EMAIL = b"""\
From: =?UTF-8?B?SsO2cmc=?= =?UTF-8?B?IFRoYWxoZWlt?= <joerg@thalheim.io>
To: someone@example.com
Subject: =?UTF-8?B?VmVyc2VuZGV0OiDigJ5BbmtlciA1NzUgVVNCLUMgSHVi4oCc?=
Date: Wed, 16 Jul 2025 16:25:33 +0000
MIME-Version: 1.0
Content-Type: text/plain; charset="utf-8"

Amazon shipment confirmation.
"""

MULTIPART_EMAIL = b"""\
From: Bob <bob@example.com>
To: joerg@thalheim.io
Subject: Meeting notes
Date: Fri, 18 Jul 2025 14:00:00 +0200
MIME-Version: 1.0
Content-Type: multipart/alternative; boundary="boundary42"

--boundary42
Content-Type: text/html; charset="utf-8"

<html><body><p>HTML version of the notes.</p></body></html>
--boundary42
Content-Type: text/plain; charset="utf-8"

Plain text version of the meeting notes.
Action items:
1. Review proposal
2. Send feedback
--boundary42--
"""

HTML_WITH_SCRIPT_EMAIL = b"""\
From: newsletter@shop.example.com
To: joerg@thalheim.io
Subject: Your order
Date: Thu, 17 Jul 2025 06:00:00 +0000
MIME-Version: 1.0
Content-Type: text/html; charset="utf-8"

<html>
<head><style>body { color: red; }</style></head>
<body>
<h1>Order Confirmation</h1>
<p>Your order #12345 has been shipped.</p>
<script>alert("xss")</script>
</body>
</html>
"""

PLAIN_EMAIL = b"""\
From: Alice <alice@example.com>
To: joerg@thalheim.io
Subject: Lunch tomorrow?
Date: Wed, 16 Jul 2025 10:00:00 +0200
Message-ID: <abc123@example.com>
MIME-Version: 1.0
Content-Type: text/plain; charset="utf-8"

Hey Joerg,

Want to grab lunch tomorrow at 12:30?

Cheers,
Alice
"""


# ── Tests ────────────────────────────────────────────────────────────────


def test_rfc2047_adjacent_encoded_words() -> None:
    """Adjacent RFC 2047 encoded-words must not get spurious spaces."""
    result = notify_flagged.decode_header("=?UTF-8?B?SGVs?==?UTF-8?B?bG8=?=")
    assert result == "Hello"


def test_rfc2047_real_from_header() -> None:
    msg = _parse(RFC2047_EMAIL)
    result = notify_flagged.decode_header(msg.get("From", ""))
    assert "Jörg" in result
    assert "Thalheim" in result


def test_rfc2047_real_subject() -> None:
    msg = _parse(RFC2047_EMAIL)
    result = notify_flagged.decode_header(msg.get("Subject", ""))
    assert "Anker 575 USB-C Hub" in result


def test_multipart_prefers_plain() -> None:
    """text/plain must win even when text/html appears first in MIME tree."""
    msg = _parse(MULTIPART_EMAIL)
    snippet = notify_flagged.get_body_snippet(msg)
    assert "Plain text version" in snippet
    assert "HTML version" not in snippet


def test_html_script_stripped() -> None:
    """Script blocks must not leak into the body snippet."""
    msg = _parse(HTML_WITH_SCRIPT_EMAIL)
    snippet = notify_flagged.get_body_snippet(msg)
    assert "Order Confirmation" in snippet
    assert "12345" in snippet
    assert "alert" not in snippet


def test_html_style_stripped() -> None:
    """Style blocks must not leak into the body snippet."""
    msg = _parse(HTML_WITH_SCRIPT_EMAIL)
    snippet = notify_flagged.get_body_snippet(msg)
    assert "color" not in snippet


def test_process_delivers_to_maildir() -> None:
    """End-to-end: email lands in Maildir and trigger line includes file path."""
    with tempfile.TemporaryDirectory() as tmp:
        maildir_path = str(Path(tmp) / "Maildir")
        pipe_path = str(Path(tmp) / "no_such_pipe")

        trigger = notify_flagged.process_flagged_email(
            PLAIN_EMAIL,
            "INBOX",
            maildir_path,
            pipe_path,
            container_maildir="/var/mail/flagged",
        )

        assert trigger is not None
        assert "Joerg starred an email" in trigger
        assert "Subject: Lunch tomorrow?" in trigger
        assert "Folder: INBOX" in trigger
        assert "File: /var/mail/flagged/new/" in trigger
        assert "\n" not in trigger

        # Email file exists on disk
        files = list((Path(tmp) / "Maildir" / "new").iterdir())
        assert len(files) == 1


def test_process_deduplicates_by_message_id() -> None:
    """Re-flagging the same email must not create a duplicate."""
    with tempfile.TemporaryDirectory() as tmp:
        maildir_path = str(Path(tmp) / "Maildir")
        pipe_path = str(Path(tmp) / "no_such_pipe")

        first = notify_flagged.process_flagged_email(
            PLAIN_EMAIL, "INBOX", maildir_path, pipe_path
        )
        second = notify_flagged.process_flagged_email(
            PLAIN_EMAIL, "INBOX", maildir_path, pipe_path
        )

        assert first is not None
        assert second is None

        files = list((Path(tmp) / "Maildir" / "new").iterdir())
        assert len(files) == 1


def test_process_writes_to_fifo() -> None:
    """Trigger line is written to the FIFO when a reader is present."""
    with tempfile.TemporaryDirectory() as tmp:
        maildir_path = str(Path(tmp) / "Maildir")
        pipe_path = str(Path(tmp) / "trigger.pipe")
        os.mkfifo(pipe_path)

        read_fd = os.open(pipe_path, os.O_RDONLY | os.O_NONBLOCK)
        try:
            notify_flagged.process_flagged_email(
                PLAIN_EMAIL, "INBOX", maildir_path, pipe_path
            )
            data = os.read(read_fd, 8192).decode()
        finally:
            os.close(read_fd)

        assert "Joerg starred an email" in data
        assert data.endswith("\n")


if __name__ == "__main__":
    import unittest

    # Auto-discover test_ functions in this module and run them.
    test_funcs = [
        obj
        for name, obj in list(globals().items())
        if name.startswith("test_") and callable(obj)
    ]
    suite = unittest.TestSuite()
    for func in test_funcs:
        suite.addTest(unittest.FunctionTestCase(func))
    unittest.TextTestRunner(verbosity=2).run(suite)
