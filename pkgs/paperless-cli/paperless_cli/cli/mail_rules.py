"""Mail rule management commands for Paperless-ngx."""

from dataclasses import dataclass
from enum import IntEnum

from paperless_cli.api import PaperlessClient
from paperless_cli.cli.formatter import print_table


class MailAction(IntEnum):
    """Mail rule action types from Paperless-ngx."""

    DELETE = 1
    MOVE = 2
    MARK_READ = 3
    FLAG = 4
    TAG = 5


@dataclass
class MailRulesListCommand:
    """Mail rules list command."""


@dataclass
class MailRulesShowCommand:
    """Mail rules show command."""

    rule_id: int


@dataclass
class MailRulesCreateCommand:
    """Mail rules create command."""

    name: str
    order: int = 0
    enabled: bool = True
    account: int | None = None
    filter_from: str | None = None
    filter_to: str | None = None
    filter_subject: str | None = None
    filter_body: str | None = None
    filter_folder: str | None = None
    rule_action: MailAction | None = None
    action_parameter: str | None = None
    assign_title_from: int | None = None
    assign_correspondent_from: int | None = None
    assign_tags: str | None = None
    assign_document_type: int | None = None
    assign_correspondent: int | None = None


@dataclass
class MailRulesUpdateCommand:
    """Mail rules update command."""

    rule_id: int
    name: str | None = None
    order: int | None = None
    enabled: bool | None = None
    account: int | None = None
    filter_from: str | None = None
    filter_to: str | None = None
    filter_subject: str | None = None
    filter_body: str | None = None
    filter_folder: str | None = None
    rule_action: MailAction | None = None
    action_parameter: str | None = None
    assign_title_from: int | None = None
    assign_correspondent_from: int | None = None
    assign_tags: str | None = None
    assign_document_type: int | None = None
    assign_correspondent: int | None = None


@dataclass
class MailRulesDeleteCommand:
    """Mail rules delete command."""

    rule_id: int
    force: bool = False


def list_mail_rules(client: PaperlessClient) -> None:
    """List all mail rules."""
    rules = client.get_mail_rules()
    if not rules:
        print("No mail rules found.")
        return

    headers = ["ID", "Name", "Order", "Account", "Filter From", "Action", "Enabled"]
    rows = []
    for rule in rules:
        account_name = "Any"
        if rule.get("account"):
            accounts = client.get_mail_accounts()
            account = next((a for a in accounts if a["id"] == rule["account"]), None)
            if account:
                account_name = account["name"]

        rows.append(
            [
                rule["id"],
                rule["name"],
                rule["order"],
                account_name,
                rule.get("filter_from") or "-",
                rule.get("action") or "default",
                "Yes" if rule.get("enabled", True) else "No",
            ]
        )
    print_table(headers, rows)


def show_mail_rule(client: PaperlessClient, rule_id: int) -> None:
    """Show details of a mail rule."""
    rule = client.get_mail_rule(rule_id)

    print(f"\nMail Rule Details (ID: {rule['id']})")
    print("=" * 50)
    print(f"Name: {rule['name']}")
    print(f"Order: {rule['order']}")
    print(f"Enabled: {'Yes' if rule.get('enabled', True) else 'No'}")

    if rule.get("account"):
        accounts = client.get_mail_accounts()
        account = next((a for a in accounts if a["id"] == rule["account"]), None)
        if account:
            print(f"Account: {account['name']}")
    else:
        print("Account: Any")

    print("\nFilters:")
    print("-" * 20)
    filters = [
        ("From", "filter_from"),
        ("To", "filter_to"),
        ("Subject", "filter_subject"),
        ("Body", "filter_body"),
        ("Folder", "filter_folder"),
    ]
    for label, key in filters:
        if rule.get(key):
            print(f"{label}: {rule[key]}")

    print("\nActions:")
    print("-" * 20)
    print(f"Action: {rule.get('action', 'default')}")
    print(f"Action Parameter: {rule.get('action_parameter', '-')}")

    print("\nMetadata Assignment:")
    print("-" * 20)
    if rule.get("assign_title_from"):
        print(f"Title From: {rule['assign_title_from']}")
    if rule.get("assign_correspondent_from"):
        print(f"Correspondent From: {rule['assign_correspondent_from']}")

    if rule.get("assign_tags"):
        tags = client.get_tags()
        tag_names = [tag["name"] for tag in tags if tag["id"] in rule["assign_tags"]]
        print(f"Tags: {', '.join(tag_names)}")

    if rule.get("assign_document_type"):
        doc_types = client.get_document_types()
        doc_type = next(
            (dt for dt in doc_types if dt["id"] == rule["assign_document_type"]),
            None,
        )
        if doc_type:
            print(f"Document Type: {doc_type['name']}")

    if rule.get("assign_correspondent"):
        correspondents = client.get_correspondents()
        correspondent = next(
            (c for c in correspondents if c["id"] == rule["assign_correspondent"]),
            None,
        )
        if correspondent:
            print(f"Correspondent: {correspondent['name']}")


def create_mail_rule(client: PaperlessClient, cmd: MailRulesCreateCommand) -> None:
    """Create a new mail rule."""
    data = {
        "name": cmd.name,
        "order": cmd.order,
        "enabled": cmd.enabled,
    }

    # Add optional fields
    if cmd.account is not None:
        data["account"] = cmd.account
    if cmd.filter_from:
        data["filter_from"] = cmd.filter_from
    if cmd.filter_to:
        data["filter_to"] = cmd.filter_to
    if cmd.filter_subject:
        data["filter_subject"] = cmd.filter_subject
    if cmd.filter_body:
        data["filter_body"] = cmd.filter_body
    if cmd.filter_folder:
        data["filter_folder"] = cmd.filter_folder
    if cmd.rule_action:
        data["action"] = cmd.rule_action.value
    if cmd.action_parameter:
        data["action_parameter"] = cmd.action_parameter
    if cmd.assign_title_from is not None:
        data["assign_title_from"] = cmd.assign_title_from
    if cmd.assign_correspondent_from is not None:
        data["assign_correspondent_from"] = cmd.assign_correspondent_from
    if cmd.assign_tags:
        data["assign_tags"] = [int(tag_id) for tag_id in cmd.assign_tags.split(",")]
    if cmd.assign_document_type is not None:
        data["assign_document_type"] = cmd.assign_document_type
    if cmd.assign_correspondent is not None:
        data["assign_correspondent"] = cmd.assign_correspondent

    rule = client.create_mail_rule(data)
    print(f"Created mail rule '{rule['name']}' with ID {rule['id']}")


def update_mail_rule(client: PaperlessClient, cmd: MailRulesUpdateCommand) -> None:
    """Update an existing mail rule."""
    # Get existing rule first
    rule = client.get_mail_rule(cmd.rule_id)

    # Update only provided fields
    if cmd.name is not None:
        rule["name"] = cmd.name
    if cmd.order is not None:
        rule["order"] = cmd.order
    if cmd.enabled is not None:
        rule["enabled"] = cmd.enabled
    if cmd.account is not None:
        rule["account"] = cmd.account
    if cmd.filter_from is not None:
        rule["filter_from"] = cmd.filter_from
    if cmd.filter_to is not None:
        rule["filter_to"] = cmd.filter_to
    if cmd.filter_subject is not None:
        rule["filter_subject"] = cmd.filter_subject
    if cmd.filter_body is not None:
        rule["filter_body"] = cmd.filter_body
    if cmd.filter_folder is not None:
        rule["filter_folder"] = cmd.filter_folder
    if cmd.rule_action is not None:
        rule["action"] = cmd.rule_action.value
    if cmd.action_parameter is not None:
        rule["action_parameter"] = cmd.action_parameter
    if cmd.assign_title_from is not None:
        rule["assign_title_from"] = cmd.assign_title_from
    if cmd.assign_correspondent_from is not None:
        rule["assign_correspondent_from"] = cmd.assign_correspondent_from
    if cmd.assign_tags is not None:
        rule["assign_tags"] = [int(tag_id) for tag_id in cmd.assign_tags.split(",")]
    if cmd.assign_document_type is not None:
        rule["assign_document_type"] = cmd.assign_document_type
    if cmd.assign_correspondent is not None:
        rule["assign_correspondent"] = cmd.assign_correspondent

    updated_rule = client.update_mail_rule(cmd.rule_id, rule)
    print(f"Updated mail rule '{updated_rule['name']}' (ID: {cmd.rule_id})")


def delete_mail_rule(client: PaperlessClient, rule_id: int, force: bool) -> None:
    """Delete a mail rule."""
    if not force:
        rule = client.get_mail_rule(rule_id)
        confirm = input(
            f"Are you sure you want to delete rule '{rule['name']}' (ID: {rule_id})? [y/N]: "
        )
        if confirm.lower() != "y":
            print("Cancelled.")
            return

    client.delete_mail_rule(rule_id)
    print(f"Deleted mail rule with ID {rule_id}")
