"""Pexpect MCP Server - Interactive process control via MCP."""

import argparse
import asyncio
import io
import os
import sys
import time
import traceback
from contextlib import redirect_stdout, suppress
from pathlib import Path
from typing import Any

import mcp.server.stdio
import pexpect
from mcp import types
from mcp.server import Server

# Module-level child process variable that persists across tool calls
_child_process: pexpect.spawn | None = None


def _get_log_path(cwd: Path | None = None) -> Path:
    """Get the log file path for the current working directory."""
    if cwd is None:
        cwd = Path.cwd()

    # Get XDG_CACHE_HOME or default to ~/.cache
    cache_dir = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache"))
    log_dir = cache_dir / "pexpect-mcp"
    log_dir.mkdir(parents=True, exist_ok=True)

    # Encode current working directory for log file name using dashes
    encoded_cwd = str(cwd).replace("/", "-").strip("-")
    return log_dir / f"{encoded_cwd}.log"


def _create_custom_print(output: io.StringIO, log_file: Any) -> Any:
    """Create a custom print function that writes to both output and log."""

    def custom_print(*args: Any, **kwargs: Any) -> None:
        """Custom print that captures output for MCP response and logs it."""
        # Convert args to string like normal print
        sep = kwargs.get("sep", " ")
        end = kwargs.get("end", "\n")
        text = sep.join(str(arg) for arg in args) + end

        # Write to capture buffer for MCP response
        output.write(text)
        # Also write to log file
        log_file.write(f"[PRINT] {text}")
        log_file.flush()

    return custom_print


def _cleanup_old_child(
    old_child: pexpect.spawn | None, new_child: pexpect.spawn | None
) -> None:
    """Terminate old child process if it was replaced."""
    if new_child is not old_child and old_child is not None:
        with suppress(Exception):
            old_child.terminate(force=True)


async def _execute_code(
    code: str,
    namespace: dict[str, Any],
    timeout: float,
    log_file: Any,  # noqa: ASYNC109
) -> None:
    """Execute Python code with timeout and stdout redirected to log."""
    with redirect_stdout(log_file):
        actual_timeout = max(timeout, 30.0)
        await asyncio.wait_for(
            asyncio.get_event_loop().run_in_executor(None, exec, code, namespace),
            timeout=actual_timeout,
        )


def _build_response(
    result: str, error: Exception | None, traceback_str: str | None, log_path: Path
) -> list[types.TextContent]:
    """Build the MCP response."""
    response = []

    if error:
        response.append(types.TextContent(type="text", text=f"Error: {error!s}"))
        if result:
            response.append(types.TextContent(type="text", text=f"Output:\n{result}"))
        if traceback_str:
            response.append(
                types.TextContent(type="text", text=f"Traceback:\n{traceback_str}")
            )
    elif result:
        response.append(types.TextContent(type="text", text=result.rstrip()))
    else:
        response.append(
            types.TextContent(
                type="text", text="Code executed successfully (no output)"
            )
        )

    # Add status info
    status_info = f"Child process: {'active' if _child_process and _child_process.isalive() else 'inactive'}"
    response.append(types.TextContent(type="text", text=status_info))
    response.append(types.TextContent(type="text", text=f"Log: {log_path}"))

    return response


"""Main entry point for the pexpect MCP server."""
# Create the MCP server
server = Server("pexpect-mcp")


@server.list_tools()
async def handle_list_tools() -> list[types.Tool]:
    """List available tools."""
    log_path = _get_log_path()

    return [
        types.Tool(
            name="run_pexpect",
            description=f"Execute Python code with pexpect library available. A persistent 'child' variable is maintained across calls. When child is reassigned, the old process is terminated. The print() function is overridden to both return output to the MCP client AND write to a log file. All stdout output is redirected to the log file. Full execution logs are saved to: {log_path} (overwritten on each call).",
            inputSchema={
                "type": "object",
                "properties": {
                    "code": {
                        "type": "string",
                        "description": "Python code to execute with pexpect library in scope",
                    },
                    "timeout": {
                        "type": "number",
                        "description": "Optional execution timeout in seconds (minimum 30s, default: no timeout)",
                    },
                },
                "required": ["code"],
            },
        )
    ]


@server.call_tool()
async def handle_call_tool(
    name: str, arguments: dict[str, Any]
) -> list[types.TextContent | types.ImageContent | types.EmbeddedResource]:
    """Execute Python code with pexpect library and persistent child variable."""
    if name != "run_pexpect":
        msg = f"Unknown tool: {name}"
        raise ValueError(msg)

    global _child_process  # noqa: PLW0603

    code = arguments.get("code", "")
    timeout = arguments.get("timeout", 30.0)
    log_path = _get_log_path()

    # Open log file in write mode (truncate on each call)
    with log_path.open("w", encoding="utf-8", buffering=1) as log_file:
        log_file.write(f"=== Pexpect MCP execution at {Path.cwd()} ===\n\n")
        log_file.write(f"[EXEC] Code:\n{code}\n\n")
        log_file.flush()

        output = io.StringIO()
        custom_print = _create_custom_print(output, log_file)

        # Create a namespace with pexpect, the global child, and custom print
        namespace: dict[str, Any] = {
            "pexpect": pexpect,
            "child": _child_process,
            "print": custom_print,
        }

        old_child = _child_process
        error = None
        traceback_str = None

        try:
            await _execute_code(code, namespace, timeout, log_file)

            # Update the module-level child variable
            _child_process = namespace.get("child", _child_process)

            # Log success
            log_file.write("\n[SUCCESS] Code executed successfully\n")
            log_file.flush()

        except Exception as e:  # noqa: BLE001
            error = e
            traceback_str = traceback.format_exc()

            # Update the module-level child variable even on error
            _child_process = namespace.get("child", _child_process)

            # Log error
            log_file.write(f"\n[ERROR] {e!s}\n")
            log_file.write(f"[TRACEBACK]\n{traceback_str}\n")
            log_file.flush()

        finally:
            # Cleanup old child if replaced
            new_child = namespace.get("child")
            _cleanup_old_child(old_child, new_child)
            _child_process = new_child

    # Build and return response
    result = output.getvalue()
    return _build_response(result, error, traceback_str, log_path)


async def main() -> None:
    # Run the server using stdio transport
    async with mcp.server.stdio.stdio_server() as (read_stream, write_stream):
        await server.run(
            read_stream, write_stream, server.create_initialization_options()
        )


def follow_logs() -> None:
    """Follow the log file for the current project."""
    log_path = _get_log_path()
    
    if not log_path.exists():
        print(f"Log file does not exist yet: {log_path}", file=sys.stderr)
        print("Waiting for log file to be created...", file=sys.stderr)
        while not log_path.exists():
            time.sleep(0.1)
    
    print(f"Following log file: {log_path}", file=sys.stderr)
    print("Press Ctrl+C to stop\n", file=sys.stderr)
    
    # Open file and seek to end initially
    with open(log_path, "r", encoding="utf-8") as f:
        # Start from beginning to show existing content
        f.seek(0)
        
        try:
            while True:
                line = f.readline()
                if line:
                    print(line, end="", flush=True)
                else:
                    # No new data, wait a bit
                    time.sleep(0.1)
        except KeyboardInterrupt:
            print("\nStopped following log file.", file=sys.stderr)
            sys.exit(0)


def run() -> None:
    """Entry point for console script."""
    parser = argparse.ArgumentParser(
        description="Pexpect MCP Server - Interactive process control via MCP"
    )
    parser.add_argument(
        "--follow",
        action="store_true",
        help="Follow the log file for the current project directory",
    )
    
    args = parser.parse_args()
    
    if args.follow:
        # Run in follow mode
        follow_logs()
    else:
        # Run as MCP server
        asyncio.run(main())


if __name__ == "__main__":
    run()
