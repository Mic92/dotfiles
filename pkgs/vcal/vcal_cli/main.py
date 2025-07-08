"""Unified vcal CLI for calendar operations."""

from __future__ import annotations

import argparse
import sys

from . import create, import_invite, reply


def main(argv: list[str] | None = None) -> int:
    """Run the main vcal CLI.

    Args:
        argv: Optional list of command line arguments (for testing).
              If None, uses sys.argv[1:].

    """
    parser = argparse.ArgumentParser(
        prog="vcal",
        description="vCalendar/iCalendar management tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Create and send a meeting invite
  vcal create -s "Team Meeting" -d 60 -a "john@example.com"

  # Import calendar invite from email
  vcal import email.eml

  # Reply to calendar invite with RSVP
  vcal reply accept < invite.eml

  # Reply with decline
  vcal reply decline -c "Sorry, I have another meeting"
""",
    )

    subparsers = parser.add_subparsers(
        title="commands",
        description="Available commands",
        help="Use 'vcal <command> --help' for command-specific help",
        dest="command",
        required=True,
    )

    # Register subcommands
    create.register_parser(subparsers)
    import_invite.register_parser(subparsers)
    reply.register_parser(subparsers)

    # Parse arguments
    args = parser.parse_args(argv)

    # Execute the selected command
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
