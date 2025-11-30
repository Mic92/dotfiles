"""Command parser for debugger-cli.

Parses simple, unambiguous commands optimized for LLM agents.
"""

import shlex
from dataclasses import dataclass
from enum import Enum, auto
from typing import Any


class CommandType(Enum):
    """Types of debugger commands."""

    # Session management
    LAUNCH = auto()
    ATTACH = auto()
    DETACH = auto()
    QUIT = auto()

    # Execution control
    CONTINUE = auto()
    STEP = auto()
    NEXT = auto()
    FINISH = auto()
    REVERSE_CONTINUE = auto()
    REVERSE_STEP = auto()
    REVERSE_NEXT = auto()

    # Breakpoints
    BREAKPOINT_SET = auto()
    BREAKPOINT_DELETE = auto()
    BREAKPOINT_LIST = auto()
    BREAKPOINT_ENABLE = auto()
    BREAKPOINT_DISABLE = auto()
    WATCHPOINT_SET = auto()

    # Inspection
    BACKTRACE = auto()
    FRAME_SELECT = auto()
    THREAD_SELECT = auto()
    THREAD_LIST = auto()
    LOCALS = auto()
    ARGS = auto()
    PRINT = auto()
    MEMORY_READ = auto()
    REGISTERS = auto()

    # Source and disassembly
    SOURCE = auto()
    DISASSEMBLE = auto()

    # Misc
    HELP = auto()
    STATUS = auto()


@dataclass
class ParsedCommand:
    """A parsed command with type and arguments."""

    type: CommandType
    args: dict[str, Any]
    raw: str


class CommandParseError(Exception):
    """Error parsing a command."""

    def __init__(self, message: str, command: str) -> None:
        super().__init__(message)
        self.message = message
        self.command = command


# Command syntax definitions
COMMAND_SYNTAX: dict[str, tuple[CommandType, str]] = {
    # Session
    "launch": (CommandType.LAUNCH, "<binary> [args...]"),
    "attach": (CommandType.ATTACH, "<pid>"),
    "detach": (CommandType.DETACH, ""),
    "quit": (CommandType.QUIT, ""),
    "exit": (CommandType.QUIT, ""),
    # Execution
    "continue": (CommandType.CONTINUE, ""),
    "c": (CommandType.CONTINUE, ""),
    "step": (CommandType.STEP, "[count]"),
    "s": (CommandType.STEP, "[count]"),
    "next": (CommandType.NEXT, "[count]"),
    "n": (CommandType.NEXT, "[count]"),
    "finish": (CommandType.FINISH, ""),
    "fin": (CommandType.FINISH, ""),
    "reverse-continue": (CommandType.REVERSE_CONTINUE, ""),
    "rc": (CommandType.REVERSE_CONTINUE, ""),
    "reverse-step": (CommandType.REVERSE_STEP, "[count]"),
    "rs": (CommandType.REVERSE_STEP, "[count]"),
    "reverse-next": (CommandType.REVERSE_NEXT, "[count]"),
    "rn": (CommandType.REVERSE_NEXT, "[count]"),
    # Breakpoints
    "break": (CommandType.BREAKPOINT_SET, "<location>"),
    "b": (CommandType.BREAKPOINT_SET, "<location>"),
    "delete": (CommandType.BREAKPOINT_DELETE, "<id>"),
    "d": (CommandType.BREAKPOINT_DELETE, "<id>"),
    "breakpoints": (CommandType.BREAKPOINT_LIST, ""),
    "bl": (CommandType.BREAKPOINT_LIST, ""),
    "enable": (CommandType.BREAKPOINT_ENABLE, "<id>"),
    "disable": (CommandType.BREAKPOINT_DISABLE, "<id>"),
    "watch": (CommandType.WATCHPOINT_SET, "<expression>"),
    # Inspection
    "backtrace": (CommandType.BACKTRACE, "[count]"),
    "bt": (CommandType.BACKTRACE, "[count]"),
    "frame": (CommandType.FRAME_SELECT, "<n>"),
    "f": (CommandType.FRAME_SELECT, "<n>"),
    "thread": (CommandType.THREAD_SELECT, "<id>"),
    "t": (CommandType.THREAD_SELECT, "<id>"),
    "threads": (CommandType.THREAD_LIST, ""),
    "locals": (CommandType.LOCALS, ""),
    "lo": (CommandType.LOCALS, ""),
    "args": (CommandType.ARGS, ""),
    "print": (CommandType.PRINT, "<expression>"),
    "p": (CommandType.PRINT, "<expression>"),
    "memory": (CommandType.MEMORY_READ, "<address> [size]"),
    "x": (CommandType.MEMORY_READ, "<address> [size]"),
    "registers": (CommandType.REGISTERS, ""),
    "reg": (CommandType.REGISTERS, ""),
    # Source
    "list": (CommandType.SOURCE, "[location]"),
    "l": (CommandType.SOURCE, "[location]"),
    "disassemble": (CommandType.DISASSEMBLE, "[location]"),
    "dis": (CommandType.DISASSEMBLE, "[location]"),
    # Misc
    "help": (CommandType.HELP, "[command]"),
    "h": (CommandType.HELP, "[command]"),
    "status": (CommandType.STATUS, ""),
}


def parse_command(line: str) -> ParsedCommand:
    """Parse a command line into a ParsedCommand.

    Args:
        line: The command line to parse

    Returns:
        A ParsedCommand with type and arguments

    Raises:
        CommandParseError: If the command is invalid
    """
    line = line.strip()
    if not line:
        raise CommandParseError("Empty command", line)

    try:
        parts = shlex.split(line)
    except ValueError as e:
        raise CommandParseError(f"Invalid syntax: {e}", line) from e

    if not parts:
        raise CommandParseError("Empty command", line)

    cmd = parts[0].lower()
    args = parts[1:]

    if cmd not in COMMAND_SYNTAX:
        raise CommandParseError(f"Unknown command: {cmd}", line)

    cmd_type, syntax = COMMAND_SYNTAX[cmd]

    # Parse arguments based on command type
    parsed_args = _parse_args(cmd_type, args, cmd, syntax)

    return ParsedCommand(type=cmd_type, args=parsed_args, raw=line)


def _parse_args(
    cmd_type: CommandType, args: list[str], cmd: str, syntax: str
) -> dict[str, Any]:
    """Parse command arguments based on command type."""
    result: dict[str, Any] = {}

    match cmd_type:
        case CommandType.LAUNCH:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            result["binary"] = args[0]
            result["args"] = args[1:]

        case CommandType.ATTACH:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            try:
                result["pid"] = int(args[0])
            except ValueError:
                raise CommandParseError(f"Invalid PID: {args[0]}", cmd)

        case CommandType.STEP | CommandType.NEXT | CommandType.REVERSE_STEP | CommandType.REVERSE_NEXT:
            result["count"] = 1
            if args:
                try:
                    result["count"] = int(args[0])
                except ValueError:
                    raise CommandParseError(f"Invalid count: {args[0]}", cmd)

        case CommandType.BREAKPOINT_SET:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            result["location"] = args[0]

        case CommandType.BREAKPOINT_DELETE | CommandType.BREAKPOINT_ENABLE | CommandType.BREAKPOINT_DISABLE:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            try:
                result["id"] = int(args[0])
            except ValueError:
                raise CommandParseError(f"Invalid breakpoint ID: {args[0]}", cmd)

        case CommandType.WATCHPOINT_SET:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            result["expression"] = " ".join(args)

        case CommandType.BACKTRACE:
            result["count"] = None
            if args:
                try:
                    result["count"] = int(args[0])
                except ValueError:
                    raise CommandParseError(f"Invalid count: {args[0]}", cmd)

        case CommandType.FRAME_SELECT:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            try:
                result["index"] = int(args[0])
            except ValueError:
                raise CommandParseError(f"Invalid frame index: {args[0]}", cmd)

        case CommandType.THREAD_SELECT:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            try:
                result["id"] = int(args[0])
            except ValueError:
                raise CommandParseError(f"Invalid thread ID: {args[0]}", cmd)

        case CommandType.PRINT:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            result["expression"] = " ".join(args)

        case CommandType.MEMORY_READ:
            if not args:
                raise CommandParseError(f"Usage: {cmd} {syntax}", cmd)
            result["address"] = args[0]
            result["size"] = 64  # Default
            if len(args) > 1:
                try:
                    result["size"] = int(args[1])
                except ValueError:
                    raise CommandParseError(f"Invalid size: {args[1]}", cmd)

        case CommandType.SOURCE | CommandType.DISASSEMBLE:
            result["location"] = args[0] if args else None

        case CommandType.HELP:
            result["command"] = args[0] if args else None

        case _:
            # Commands with no arguments
            pass

    return result


def get_help_text(command: str | None = None) -> str:
    """Get help text for commands."""
    if command:
        cmd = command.lower()
        if cmd in COMMAND_SYNTAX:
            cmd_type, syntax = COMMAND_SYNTAX[cmd]
            return f"{cmd} {syntax}".strip()
        return f"Unknown command: {command}"

    # Full help
    lines = ["Available commands:", ""]

    categories = [
        ("Session", [CommandType.LAUNCH, CommandType.ATTACH, CommandType.DETACH, CommandType.QUIT]),
        (
            "Execution",
            [
                CommandType.CONTINUE,
                CommandType.STEP,
                CommandType.NEXT,
                CommandType.FINISH,
                CommandType.REVERSE_CONTINUE,
                CommandType.REVERSE_STEP,
                CommandType.REVERSE_NEXT,
            ],
        ),
        (
            "Breakpoints",
            [
                CommandType.BREAKPOINT_SET,
                CommandType.BREAKPOINT_DELETE,
                CommandType.BREAKPOINT_LIST,
                CommandType.BREAKPOINT_ENABLE,
                CommandType.BREAKPOINT_DISABLE,
                CommandType.WATCHPOINT_SET,
            ],
        ),
        (
            "Inspection",
            [
                CommandType.BACKTRACE,
                CommandType.FRAME_SELECT,
                CommandType.THREAD_SELECT,
                CommandType.THREAD_LIST,
                CommandType.LOCALS,
                CommandType.ARGS,
                CommandType.PRINT,
                CommandType.MEMORY_READ,
                CommandType.REGISTERS,
            ],
        ),
        ("Source", [CommandType.SOURCE, CommandType.DISASSEMBLE]),
        ("Misc", [CommandType.HELP, CommandType.STATUS]),
    ]

    for category, types in categories:
        lines.append(f"{category}:")
        for cmd_type in types:
            # Find primary command for this type
            for cmd, (t, syntax) in COMMAND_SYNTAX.items():
                if t == cmd_type and len(cmd) > 2:  # Skip short aliases
                    lines.append(f"  {cmd} {syntax}".rstrip())
                    break
        lines.append("")

    return "\n".join(lines)
