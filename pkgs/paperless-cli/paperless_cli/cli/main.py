"""
Paperless-ngx CLI for managing documents, mail accounts, mail rules, and tags.
"""

import argparse
import json
import logging
import os
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, cast

from paperless_cli.api import PaperlessAPIError, PaperlessClient
from paperless_cli.cli.documents import (
    DocumentsDeleteCommand,
    DocumentsGetCommand,
    DocumentsSearchCommand,
    DocumentsUploadCommand,
    delete_document,
    get_document,
    search_documents,
    upload_document,
)
from paperless_cli.cli.mail_accounts import (
    MailAccountsCommand,
    list_mail_accounts,
)
from paperless_cli.cli.mail_rules import (
    MailAction,
    MailRulesCreateCommand,
    MailRulesDeleteCommand,
    MailRulesListCommand,
    MailRulesShowCommand,
    MailRulesUpdateCommand,
    create_mail_rule,
    delete_mail_rule,
    list_mail_rules,
    show_mail_rule,
    update_mail_rule,
)
from paperless_cli.cli.tags import (
    TagsCreateCommand,
    TagsDeleteCommand,
    TagsListCommand,
    create_tag,
    delete_tag,
    list_tags,
)

# Module-level logger
logger = logging.getLogger(__name__)


Command = (
    MailAccountsCommand
    | TagsListCommand
    | TagsCreateCommand
    | TagsDeleteCommand
    | MailRulesListCommand
    | MailRulesShowCommand
    | MailRulesCreateCommand
    | MailRulesUpdateCommand
    | MailRulesDeleteCommand
    | DocumentsSearchCommand
    | DocumentsGetCommand
    | DocumentsUploadCommand
    | DocumentsDeleteCommand
)


@dataclass
class Options:
    """Parsed command line options."""

    url: str | None = None
    token: str | None = None
    token_command: str | None = None
    debug: bool = False
    command: Command | None = None


def load_config() -> dict[str, Any]:
    """Load configuration from XDG_CONFIG_HOME/paperless-cli/config.json."""
    xdg_config_home = os.environ.get("XDG_CONFIG_HOME", str(Path.home() / ".config"))
    config_dir = Path(xdg_config_home) / "paperless-cli"
    config_file = config_dir / "config.json"

    if config_file.exists():
        with config_file.open() as f:
            return cast("dict[str, Any]", json.load(f))
    return {}


def get_token(token: str | None, token_command: str | None) -> str | None:
    """Get token from direct value or by running a command."""
    if token:
        return token

    if token_command:
        try:
            result = subprocess.run(  # noqa: S602
                token_command, shell=True, capture_output=True, text=True, check=True
            )
            return result.stdout.strip()
        except subprocess.CalledProcessError as e:
            print(f"Error running token command: {e}")
            return None

    return None


def parse_args() -> Options:
    """Parse command line arguments and return Options."""
    parser = argparse.ArgumentParser(description="Paperless-ngx CLI")
    parser.add_argument(
        "--url",
        default=os.environ.get("PAPERLESS_URL"),
        help="Paperless-ngx URL (or set PAPERLESS_URL env var)",
    )
    parser.add_argument(
        "--token",
        default=os.environ.get("PAPERLESS_TOKEN"),
        help="API token (or set PAPERLESS_TOKEN env var)",
    )
    parser.add_argument(
        "--token-command",
        help="Command to run to get the API token",
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug output",
    )

    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # Mail accounts command
    accounts_parser = subparsers.add_parser("mail-accounts", help="List mail accounts")
    accounts_parser.add_argument(
        "action", choices=["list"], default="list", nargs="?", help="Action to perform"
    )

    # Tags commands
    tags_parser = subparsers.add_parser("tags", help="Manage tags")
    tags_subparsers = tags_parser.add_subparsers(dest="action", help="Tags actions")

    # List tags
    tags_subparsers.add_parser("list", help="List all tags")

    # Create tag
    create_tag_parser = tags_subparsers.add_parser("create", help="Create a new tag")
    create_tag_parser.add_argument("name", help="Tag name")
    create_tag_parser.add_argument("--color", help="Tag color (hex format)")

    # Delete tag
    delete_tag_parser = tags_subparsers.add_parser("delete", help="Delete a tag")
    delete_tag_parser.add_argument("tag_id", type=int, help="Tag ID")
    delete_tag_parser.add_argument("-f", "--force", action="store_true", help="Skip confirmation")

    # Mail rules commands
    rules_parser = subparsers.add_parser("mail-rules", help="Manage mail rules")
    rules_subparsers = rules_parser.add_subparsers(dest="action", help="Mail rules actions")

    # List rules
    rules_subparsers.add_parser("list", help="List all mail rules")

    # Show rule
    show_parser = rules_subparsers.add_parser("show", help="Show mail rule details")
    show_parser.add_argument("rule_id", type=int, help="Rule ID")

    # Create rule
    create_parser = rules_subparsers.add_parser("create", help="Create a new mail rule")
    create_parser.add_argument("name", help="Rule name")
    create_parser.add_argument("--order", type=int, default=0, help="Rule order")
    create_parser.add_argument("--enabled", action="store_true", default=True, help="Enable rule")
    create_parser.add_argument(
        "--disabled", dest="enabled", action="store_false", help="Disable rule"
    )
    create_parser.add_argument("--account", type=int, help="Mail account ID")
    create_parser.add_argument("--filter-from", help="Filter by sender")
    create_parser.add_argument("--filter-to", help="Filter by recipient")
    create_parser.add_argument("--filter-subject", help="Filter by subject")
    create_parser.add_argument("--filter-body", help="Filter by body")
    create_parser.add_argument("--filter-folder", help="Filter by folder")
    create_parser.add_argument(
        "--rule-action",
        type=lambda x: MailAction[x.upper()],
        choices=list(MailAction),
        help="Action to perform",
    )
    create_parser.add_argument("--action-parameter", help="Action parameter")
    create_parser.add_argument(
        "--assign-title-from",
        type=int,
        help="Assign title from (0=from, 1=subject, 2=filename)",
    )
    create_parser.add_argument(
        "--assign-correspondent-from",
        type=int,
        help="Assign correspondent from (0=from, 1=to, 2=custom)",
    )
    create_parser.add_argument("--assign-tags", help="Comma-separated list of tag IDs to assign")
    create_parser.add_argument(
        "--assign-document-type", type=int, help="Document type ID to assign"
    )
    create_parser.add_argument(
        "--assign-correspondent", type=int, help="Correspondent ID to assign"
    )

    # Update rule
    update_parser = rules_subparsers.add_parser("update", help="Update an existing mail rule")
    update_parser.add_argument("rule_id", type=int, help="Rule ID")
    update_parser.add_argument("--name", help="New rule name")
    update_parser.add_argument("--order", type=int, help="New rule order")
    update_parser.add_argument("--enabled", action="store_true", help="Enable rule")
    update_parser.add_argument(
        "--disabled", dest="enabled", action="store_false", help="Disable rule"
    )
    update_parser.add_argument("--account", type=int, help="Mail account ID")
    update_parser.add_argument("--filter-from", help="Filter by sender")
    update_parser.add_argument("--filter-to", help="Filter by recipient")
    update_parser.add_argument("--filter-subject", help="Filter by subject")
    update_parser.add_argument("--filter-body", help="Filter by body")
    update_parser.add_argument("--filter-folder", help="Filter by folder")
    update_parser.add_argument(
        "--rule-action",
        type=lambda x: MailAction[x.upper()],
        choices=list(MailAction),
        help="Action to perform",
    )
    update_parser.add_argument("--action-parameter", help="Action parameter")
    update_parser.add_argument(
        "--assign-title-from",
        type=int,
        help="Assign title from (0=from, 1=subject, 2=filename)",
    )
    update_parser.add_argument(
        "--assign-correspondent-from",
        type=int,
        help="Assign correspondent from (0=from, 1=to, 2=custom)",
    )
    update_parser.add_argument("--assign-tags", help="Comma-separated list of tag IDs to assign")
    update_parser.add_argument(
        "--assign-document-type", type=int, help="Document type ID to assign"
    )
    update_parser.add_argument(
        "--assign-correspondent", type=int, help="Correspondent ID to assign"
    )

    # Delete rule
    delete_parser = rules_subparsers.add_parser("delete", help="Delete a mail rule")
    delete_parser.add_argument("rule_id", type=int, help="Rule ID")
    delete_parser.add_argument("-f", "--force", action="store_true", help="Skip confirmation")

    # Documents commands
    docs_parser = subparsers.add_parser("documents", help="Manage documents")
    docs_subparsers = docs_parser.add_subparsers(dest="action", help="Documents actions")

    # Search documents
    search_parser = docs_subparsers.add_parser("search", help="Search documents")
    search_parser.add_argument("query", nargs="?", help="Search query")
    search_parser.add_argument("--page", type=int, default=1, help="Page number")
    search_parser.add_argument("--page-size", type=int, default=25, help="Results per page")

    # Get document
    get_parser = docs_subparsers.add_parser("get", help="Get document details or download")
    get_parser.add_argument("document_id", type=int, help="Document ID")
    get_parser.add_argument("--download", action="store_true", help="Download the document")
    get_parser.add_argument(
        "--original", action="store_true", help="Download original if available"
    )
    get_parser.add_argument("-o", "--output", help="Output filename for download")
    get_parser.add_argument("--metadata", action="store_true", help="Show document metadata")

    # Upload document
    upload_parser = docs_subparsers.add_parser("upload", help="Upload a document")
    upload_parser.add_argument("file_path", help="Path to the document file")
    upload_parser.add_argument("--title", help="Document title")
    upload_parser.add_argument("--tags", help="Comma-separated list of tag IDs")

    # Delete document
    delete_doc_parser = docs_subparsers.add_parser("delete", help="Delete a document")
    delete_doc_parser.add_argument("document_id", type=int, help="Document ID")
    delete_doc_parser.add_argument("-f", "--force", action="store_true", help="Skip confirmation")

    # Parse arguments
    args = parser.parse_args()

    # Create Options
    options = Options(
        url=args.url,
        token=args.token,
        token_command=args.token_command,
        debug=args.debug,
    )

    # Create appropriate command object
    if args.command == "mail-accounts":
        options.command = MailAccountsCommand(action=getattr(args, "action", "list") or "list")
    elif args.command == "tags":
        action = getattr(args, "action", None)
        if action == "create":
            options.command = TagsCreateCommand(name=args.name, color=args.color)
        elif action == "delete":
            options.command = TagsDeleteCommand(tag_id=args.tag_id, force=args.force)
        else:  # list or None
            options.command = TagsListCommand()
    elif args.command == "mail-rules":
        action = getattr(args, "action", None)
        if action == "create":
            options.command = MailRulesCreateCommand(
                name=args.name,
                order=args.order,
                enabled=args.enabled,
                account=args.account,
                filter_from=args.filter_from,
                filter_to=args.filter_to,
                filter_subject=args.filter_subject,
                filter_body=args.filter_body,
                filter_folder=args.filter_folder,
                rule_action=args.rule_action if hasattr(args, "rule_action") else None,
                action_parameter=getattr(args, "action_parameter", None),
                assign_title_from=args.assign_title_from,
                assign_correspondent_from=args.assign_correspondent_from,
                assign_tags=args.assign_tags,
                assign_document_type=args.assign_document_type,
                assign_correspondent=args.assign_correspondent,
            )
        elif action == "show":
            options.command = MailRulesShowCommand(rule_id=args.rule_id)
        elif action == "update":
            options.command = MailRulesUpdateCommand(
                rule_id=args.rule_id,
                name=getattr(args, "name", None),
                order=getattr(args, "order", None),
                enabled=getattr(args, "enabled", None),
                account=getattr(args, "account", None),
                filter_from=getattr(args, "filter_from", None),
                filter_to=getattr(args, "filter_to", None),
                filter_subject=getattr(args, "filter_subject", None),
                filter_body=getattr(args, "filter_body", None),
                filter_folder=getattr(args, "filter_folder", None),
                rule_action=getattr(args, "rule_action", None),
                action_parameter=getattr(args, "action_parameter", None),
                assign_title_from=getattr(args, "assign_title_from", None),
                assign_correspondent_from=getattr(args, "assign_correspondent_from", None),
                assign_tags=getattr(args, "assign_tags", None),
                assign_document_type=getattr(args, "assign_document_type", None),
                assign_correspondent=getattr(args, "assign_correspondent", None),
            )
        elif action == "delete":
            options.command = MailRulesDeleteCommand(rule_id=args.rule_id, force=args.force)
        else:  # list or None
            options.command = MailRulesListCommand()
    elif args.command == "documents":
        action = getattr(args, "action", None)
        if action == "search":
            options.command = DocumentsSearchCommand(
                query=getattr(args, "query", None), page=args.page, page_size=args.page_size
            )
        elif action == "get":
            options.command = DocumentsGetCommand(
                document_id=args.document_id,
                download=args.download,
                original=args.original,
                output=args.output,
                metadata=args.metadata,
            )
        elif action == "upload":
            options.command = DocumentsUploadCommand(
                file_path=args.file_path,
                title=getattr(args, "title", None),
                tags=getattr(args, "tags", None),
            )
        elif action == "delete":
            options.command = DocumentsDeleteCommand(document_id=args.document_id, force=args.force)

    return options


def main() -> None:
    """Main entry point."""
    options = parse_args()

    # Configure logging
    if options.debug:
        logging.basicConfig(level=logging.DEBUG, format="%(levelname)s: %(message)s")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    # Debug logging
    logger.debug(f"Options: {options}")
    logger.debug(f"Command: {options.command}")

    if not options.command:
        # Create parser just for help
        parser = argparse.ArgumentParser(description="Paperless-ngx CLI")
        parser.print_help()
        sys.exit(1)

    # Load config file
    config = load_config()

    # Get URL from args, env, or config
    url = options.url or config.get("url")
    if not url:
        print("Error: Paperless URL not provided. Use --url, set PAPERLESS_URL, or add to config")
        sys.exit(1)

    # Get token from args, env, command, or config
    token = get_token(options.token, options.token_command or config.get("token_command"))
    if not token:
        print(
            "Error: API token not provided. Use --token, set PAPERLESS_TOKEN, --token-command, or add to config"
        )
        sys.exit(1)

    try:
        client = PaperlessClient(url, token)

        # Handle commands using pattern matching
        match options.command:
            case MailAccountsCommand():
                list_mail_accounts(client)
            case TagsListCommand():
                list_tags(client)
            case TagsCreateCommand() as cmd:
                create_tag(client, cmd.name, cmd.color)
            case TagsDeleteCommand() as cmd:
                delete_tag(client, cmd.tag_id, cmd.force)
            case MailRulesListCommand():
                list_mail_rules(client)
            case MailRulesShowCommand() as cmd:
                show_mail_rule(client, cmd.rule_id)
            case MailRulesCreateCommand() as cmd:
                create_mail_rule(client, cmd)
            case MailRulesUpdateCommand() as cmd:
                update_mail_rule(client, cmd)
            case MailRulesDeleteCommand() as cmd:
                delete_mail_rule(client, cmd.rule_id, cmd.force)
            case DocumentsSearchCommand() as cmd:
                search_documents(client, cmd)
            case DocumentsGetCommand() as cmd:
                get_document(client, cmd)
            case DocumentsUploadCommand() as cmd:
                upload_document(client, cmd)
            case DocumentsDeleteCommand() as cmd:
                delete_document(client, cmd.document_id, cmd.force)
            case _:
                print("Unknown command")
                sys.exit(1)
    except PaperlessAPIError as e:
        print(f"API Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
