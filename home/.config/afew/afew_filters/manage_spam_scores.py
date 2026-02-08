#!/usr/bin/env python3
"""Utility script to manage afew spam scores database."""

import argparse
from dataclasses import dataclass
from enum import StrEnum

from afew_filters.spam_database import SpamDatabase


class OrderBy(StrEnum):
    """Enum for ordering options."""

    SCORE = "score"
    EMAIL = "email"
    LAST_SEEN = "last_seen"
    TOTAL_MESSAGES = "total_messages"


@dataclass
class Options:
    """Parsed command line options."""

    command: str | None = None
    # List command options
    limit: int | None = None
    order: OrderBy = OrderBy.SCORE
    # Reset/Set command options
    email: str | None = None
    # Set command options
    score: float | None = None


def list_scores(limit: int | None = None, order: OrderBy = OrderBy.SCORE) -> None:
    """List email addresses with their spam scores."""
    db = SpamDatabase()
    if not db.db_path.exists():
        print("No spam scores database found.")
        return

    with db.get_connection() as conn:
        # Build query safely to avoid SQL injection
        base_query = """
            SELECT email, score, spam_count, ham_count, total_messages, last_seen
            FROM spam_scores
        """

        # Safe order by using enum values
        match order:
            case OrderBy.EMAIL:
                base_query += " ORDER BY email DESC"
            case OrderBy.LAST_SEEN:
                base_query += " ORDER BY last_seen DESC"
            case OrderBy.TOTAL_MESSAGES:
                base_query += " ORDER BY total_messages DESC"
            case OrderBy.SCORE | _:
                base_query += " ORDER BY score DESC"

        if limit:
            base_query += " LIMIT ?"
            cursor = conn.execute(base_query, (limit,))
        else:
            cursor = conn.execute(base_query)
        print(
            f"{'Email':<40} {'Score':>8} {'Spam':>6} {'Ham':>6} {'Total':>7} {'Last Seen'}"
        )
        print("-" * 80)

        for row in cursor:
            email, score, spam_count, ham_count, total, last_seen = row
            print(
                f"{email:<40} {score:>8.2f} {spam_count:>6} {ham_count:>6} {total:>7} {last_seen}"
            )


def reset_score(email: str) -> None:
    """Reset the spam score for an email address."""
    db = SpamDatabase()
    if not db.db_path.exists():
        print("No spam scores database found.")
        return

    with db.get_connection() as conn:
        conn.execute("DELETE FROM spam_scores WHERE email = ?", (email.lower(),))
        if conn.total_changes > 0:
            print(f"Reset score for {email}")
            conn.commit()
        else:
            print(f"No score found for {email}")


def set_score(email: str, score: float) -> None:
    """Manually set the spam score for an email address."""
    db = SpamDatabase()
    db.init_database()  # Ensure database exists

    with db.get_connection() as conn:
        # Set manual score
        spam_count = 1 if score > 0 else 0
        ham_count = 0 if score > 0 else 1

        conn.execute(
            """INSERT OR REPLACE INTO spam_scores
               (email, score, spam_count, ham_count)
               VALUES (?, ?, ?, ?)""",
            (email.lower(), score, spam_count, ham_count),
        )
        conn.commit()
        print(f"Set score for {email} to {score}")


def show_stats() -> None:
    """Show database statistics."""
    db = SpamDatabase()
    if not db.db_path.exists():
        print("No spam scores database found.")
        return

    with db.get_connection() as conn:
        # Total emails
        cursor = conn.execute("SELECT COUNT(*) FROM spam_scores")
        total_emails = cursor.fetchone()[0]

        # Spam vs ham
        cursor = conn.execute("SELECT COUNT(*) FROM spam_scores WHERE score >= 2.0")
        spam_emails = cursor.fetchone()[0]

        cursor = conn.execute("SELECT COUNT(*) FROM spam_scores WHERE score <= -2.0")
        ham_emails = cursor.fetchone()[0]

        cursor = conn.execute(
            "SELECT COUNT(*) FROM spam_scores WHERE score > -2.0 AND score < 2.0"
        )
        uncertain_emails = cursor.fetchone()[0]

        # Total messages processed
        cursor = conn.execute("SELECT SUM(total_messages) FROM spam_scores")
        total_messages = cursor.fetchone()[0] or 0

        print("Database Statistics:")
        print(f"  Total email addresses: {total_emails}")
        print(f"  Spam addresses (score >= 2.0): {spam_emails}")
        print(f"  Ham addresses (score <= -2.0): {ham_emails}")
        print(f"  Uncertain addresses: {uncertain_emails}")
        print(f"  Total messages processed: {total_messages}")


def parse_args() -> tuple[Options, argparse.ArgumentParser]:
    """Parse command line arguments and return Options dataclass and parser."""
    parser = argparse.ArgumentParser(
        description="Manage afew Claude spam filter scores"
    )
    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # List command
    list_parser = subparsers.add_parser("list", help="List email scores")
    list_parser.add_argument("-n", "--limit", type=int, help="Limit number of results")
    list_parser.add_argument(
        "-o",
        "--order",
        choices=[e.value for e in OrderBy],
        default=OrderBy.SCORE.value,
        help="Order results by field",
    )

    # Reset command
    reset_parser = subparsers.add_parser("reset", help="Reset score for an email")
    reset_parser.add_argument("email", help="Email address to reset")

    # Set command
    set_parser = subparsers.add_parser("set", help="Manually set score for an email")
    set_parser.add_argument("email", help="Email address")
    set_parser.add_argument("score", type=float, help="Score to set")

    # Stats command
    subparsers.add_parser("stats", help="Show database statistics")

    args = parser.parse_args()

    # Build Options dataclass
    options = Options(command=args.command)

    match args.command:
        case "list":
            options.limit = args.limit
            options.order = OrderBy(args.order)
        case "reset":
            options.email = args.email
        case "set":
            options.email = args.email
            options.score = args.score
        # stats has no additional options

    return options, parser


def main() -> None:
    """Main entry point."""
    options, parser = parse_args()

    match options.command:
        case "list":
            list_scores(options.limit, options.order)
        case "reset":
            assert options.email is not None
            reset_score(options.email)
        case "set":
            assert options.email is not None
            assert options.score is not None
            set_score(options.email, options.score)
        case "stats":
            show_stats()
        case _:
            # Print help if no command specified
            parser.print_help()


if __name__ == "__main__":
    main()
