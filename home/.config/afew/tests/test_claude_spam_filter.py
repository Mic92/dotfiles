"""Integration test for ClaudeSpamFilter using example emails."""

import shutil
import sqlite3
import subprocess

# Import the filter
import sys
from pathlib import Path
from typing import Any

import pytest
from afew.Database import Database  # type: ignore[import-untyped]
from notmuch import Database as NotmuchDatabase  # type: ignore[import-untyped]
from notmuch.message import Message  # type: ignore[import-untyped]

sys.path.insert(0, str(Path(__file__).parent.parent))
from afew_filters.claude_spam_filter import ClaudeSpamFilter


@pytest.fixture
def test_maildir(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> tuple[Path, Path]:
    """Create a test maildir with notmuch database."""
    # Set up test environment
    monkeypatch.setenv("XDG_DATA_HOME", str(tmp_path))

    # Create maildir structure
    maildir = tmp_path / "mail"
    maildir.mkdir()
    cur_dir = maildir / "cur"
    cur_dir.mkdir()
    new_dir = maildir / "new"
    new_dir.mkdir()
    tmp_dir = maildir / "tmp"
    tmp_dir.mkdir()

    # Create notmuch config
    notmuch_config = tmp_path / ".notmuch-config"
    notmuch_config.write_text(f"""[database]
path={maildir}

[user]
name=Test User
primary_email=test@example.com

[new]
tags=unread;inbox;
""")

    # Set NOTMUCH_CONFIG to use our test config
    monkeypatch.setenv("NOTMUCH_CONFIG", str(notmuch_config))
    # Also set MAILDIR for afew Database wrapper
    monkeypatch.setenv("MAILDIR", str(maildir))

    # Initialize notmuch database
    subprocess.run(["notmuch", "new", "--quiet"], check=True)

    return tmp_path, maildir


def create_test_emails(test_dir: Path) -> None:
    """Create test email files."""
    test_dir.mkdir(exist_ok=True)

    # Example spam email 1 - Nigerian prince scam
    spam1 = test_dir / "spam1.eml"
    spam1.write_text("""From: Prince Abubakar <prince.abubakar@totallylegit.ng>
To: user@example.com
Subject: URGENT: $45 MILLION USD INHERITANCE
Date: Mon, 15 Jan 2024 10:00:00 +0000
Content-Type: text/plain

Dear Beneficiary,

I am Prince Abubakar, son of late King of Nigeria. I have $45 MILLION USD
in bank account that I need to transfer urgently. You have been selected
as beneficiary. Please send your bank details and $500 processing fee.

This is 100% legitimate and risk free opportunity!!!

Regards,
Prince Abubakar
""")

    # Example spam email 2 - Crypto scam
    spam2 = test_dir / "spam2.eml"
    spam2.write_text("""From: "Crypto Profits" <noreply@get-rich-quick.xyz>
To: victim@example.com
Subject: You've Been Selected! Claim Your FREE Bitcoin NOW!
Date: Tue, 16 Jan 2024 14:30:00 +0000
Reply-To: scammer@differentdomain.com
List-Unsubscribe: <http://suspicious-link.xyz/unsubscribe>
Content-Type: text/html

<html>
<body>
<h1>CONGRATULATIONS!</h1>
<p>You've been randomly selected to receive 5 FREE BITCOIN!</p>
<p>Click here to claim: http://totallynotascam.xyz/claim</p>
<p>Act NOW! This offer expires in 24 HOURS!</p>
<p><small>To unsubscribe, click here</small></p>
</body>
</html>
""")

    # Example ham email - legitimate newsletter
    ham1 = test_dir / "ham1.eml"
    ham1.write_text("""From: Python Weekly <noreply@pythonweekly.com>
To: subscriber@example.com
Subject: Python Weekly - Issue #523
Date: Thu, 18 Jan 2024 09:00:00 +0000
List-Unsubscribe: <https://pythonweekly.com/unsubscribe>
Content-Type: text/plain

Python Weekly - Issue #523

Featured Articles:
- Understanding Python's GIL
- Best Practices for Async Programming
- New Features in Python 3.12

View online: https://pythonweekly.com/issues/523

To unsubscribe: https://pythonweekly.com/unsubscribe
""")

    # Example ham email - personal email
    ham2 = test_dir / "ham2.eml"
    ham2.write_text("""From: John Doe <john.doe@company.com>
To: jane.smith@company.com
Subject: Meeting notes from today
Date: Fri, 19 Jan 2024 15:45:00 +0000
Content-Type: text/plain

Hi Jane,

Here are the notes from our meeting today:

1. Project timeline needs to be updated
2. Budget approval pending from finance
3. Next milestone: February 15th

Let me know if I missed anything.

Best,
John
""")

    # Example spam email 3 - Phishing
    spam3 = test_dir / "spam3.eml"
    spam3.write_text("""From: "Amazon Support" <no-reply@amaz0n-security.com>
To: customer@example.com
Subject: Your Amazon Account Has Been Locked
Date: Sat, 20 Jan 2024 03:00:00 +0000
Reply-To: support@sketchy-domain.ru
Content-Type: text/plain

Dear Customer,

We detected suspicious activity on your Amazon account. Your account
has been temporarily locked for your protection.

To unlock your account, please verify your identity by clicking here:
http://not-amazon.com/verify-account

You must complete this within 24 hours or your account will be permanently deleted.

Amazon Security Team
""")


@pytest.fixture
def test_emails() -> list[tuple[str, str, str]]:
    """Provide test email data."""
    return [
        ("spam1.eml", "test-spam-1@example.com", "prince.abubakar@totallylegit.ng"),
        (
            "spam2.eml",
            "test-spam-2@example.com",
            '"Crypto Profits" <noreply@get-rich-quick.xyz>',
        ),
        (
            "ham1.eml",
            "test-ham-1@example.com",
            "Python Weekly <noreply@pythonweekly.com>",
        ),
        ("ham2.eml", "test-ham-2@example.com", "John Doe <john.doe@company.com>"),
        (
            "spam3.eml",
            "test-spam-3@example.com",
            '"Amazon Support" <no-reply@amaz0n-security.com>',
        ),
    ]


def setup_test_emails_in_maildir(
    tmp_path: Path, maildir: Path
) -> list[tuple[str, Path]]:
    """Create and copy test emails to maildir."""
    test_email_dir = tmp_path / "test_emails"
    create_test_emails(test_email_dir)

    cur_dir = maildir / "cur"
    test_files = []
    for i, eml_file in enumerate(test_email_dir.glob("*.eml")):
        maildir_name = f"1234567890.{i}.test:2,S"
        dest = cur_dir / maildir_name
        shutil.copy(eml_file, dest)
        test_files.append((eml_file.name, dest))

    subprocess.run(["notmuch", "new", "--quiet"], check=True)
    return test_files


def find_message_by_filename(db: NotmuchDatabase, mail_file: Path) -> Message | None:
    """Find notmuch message by filename."""
    query = db.create_query("*")
    messages = list(query.search_messages())

    for msg in messages:
        if msg.get_filename().endswith(mail_file.name):
            return msg
    return None


def process_single_message(
    filter_instance: ClaudeSpamFilter,
    db: NotmuchDatabase,
    orig_name: str,
    mail_file: Path,
) -> dict[str, Any]:
    """Process a single test message and return results."""
    message = find_message_by_filename(db, mail_file)
    assert message, f"Message {orig_name} not found in notmuch database"

    from_addr = message.get_header("From")
    initial_tags = set(message.get_tags())

    print(f"  Initial tags: {sorted(initial_tags)}")

    # Process the message
    filter_instance.handle_message(message)
    message.tags_to_maildir_flags()

    final_tags = set(message.get_tags())
    new_tags = final_tags - initial_tags

    print(f"\n{orig_name}: {from_addr}")
    print(f"  Added tags: {sorted(new_tags)}")

    return {"file": orig_name, "from": from_addr, "new_tags": new_tags}


def verify_results_after_commit(
    afew_db: Database, test_files: list[tuple[str, Path]], results: list[dict[str, Any]]
) -> None:
    """Verify tags after commit and update results."""
    afew_db.close()
    db = afew_db.open()

    for i, (orig_name, mail_file) in enumerate(test_files):
        message = find_message_by_filename(db, mail_file)
        final_tags = set(message.get_tags())
        initial_tags = {"inbox", "unread"}  # from notmuch config
        new_tags = final_tags - initial_tags

        results[i]["new_tags"] = new_tags
        print(f"\n{orig_name}: After commit, tags = {sorted(new_tags)}")


def test_claude_spam_filter(
    test_maildir: tuple[Path, Path], test_emails: list[tuple[str, str, str]]
) -> None:
    """Test ClaudeSpamFilter with example emails."""
    tmp_path, maildir = test_maildir

    # Setup
    test_files = setup_test_emails_in_maildir(tmp_path, maildir)
    afew_db = Database()
    filter_instance = ClaudeSpamFilter(afew_db)
    db = afew_db.open()

    print(f"Filter query: {filter_instance.query}")

    # Process messages
    results = []
    for orig_name, mail_file in test_files:
        result = process_single_message(filter_instance, db, orig_name, mail_file)
        results.append(result)

    # Commit and verify
    filter_instance.commit(dry_run=False)
    assert isinstance(filter_instance.khard_contacts, set)

    verify_results_after_commit(afew_db, test_files, results)

    # Assertions
    assert any("spam" in r["new_tags"] for r in results if "spam" in r["file"])
    assert any("ham" in r["new_tags"] for r in results if "ham" in r["file"])

    # Check database
    db_path = tmp_path / "afew" / "spam_scores.sqlite"
    assert db_path.exists()

    with sqlite3.connect(db_path) as conn:
        cursor = conn.execute("SELECT COUNT(*) FROM spam_scores")
        count = cursor.fetchone()[0]
        assert count > 0
