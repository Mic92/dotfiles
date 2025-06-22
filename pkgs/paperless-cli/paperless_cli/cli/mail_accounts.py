"""Mail account management commands for Paperless-ngx."""

from dataclasses import dataclass

from paperless_cli.api import PaperlessClient
from paperless_cli.cli.formatter import print_table


@dataclass
class MailAccountsCommand:
    """Mail accounts command."""

    action: str = "list"


def list_mail_accounts(client: PaperlessClient) -> None:
    """List all mail accounts."""
    accounts = client.get_mail_accounts()
    if not accounts:
        print("No mail accounts found.")
        return

    headers = ["ID", "Name", "IMAP Server", "Username", "Folder"]
    rows = [
        [
            account["id"],
            account["name"],
            account["imap_server"],
            account["username"],
            account.get("folder", "INBOX"),
        ]
        for account in accounts
    ]
    print_table(headers, rows)
