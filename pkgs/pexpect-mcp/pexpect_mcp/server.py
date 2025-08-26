"""Pexpect MCP Server - Interactive process control via MCP."""

import asyncio
import io
import traceback
from contextlib import redirect_stdout, suppress
from typing import Any

import mcp.server.stdio
import pexpect
from mcp import types
from mcp.server import Server

# Module-level child process variable that persists across tool calls
_child_process: pexpect.spawn | None = None

"""Main entry point for the pexpect MCP server."""
# Create the MCP server
server = Server("pexpect-mcp")


@server.list_tools()
async def handle_list_tools() -> list[types.Tool]:
    """List available tools."""
    return [
        types.Tool(
            name="run_pexpect",
            description="Execute Python code with pexpect library available. A persistent 'child' variable is maintained across calls. When child is reassigned, the old process is terminated.",
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

    output = io.StringIO()

    # Create a namespace with pexpect, the global child
    namespace: dict[str, Any] = {
        "pexpect": pexpect,
        "child": _child_process,
    }

    old_child = _child_process

    try:
        with redirect_stdout(output):
            # Run with timeout (minimum 30s)
            actual_timeout = max(timeout, 30.0)
            await asyncio.wait_for(
                asyncio.get_event_loop().run_in_executor(None, exec, code, namespace),
                timeout=actual_timeout,
            )

        # Check if child was reassigned
        new_child = namespace.get("child")
        if new_child is not old_child and old_child is not None:
            # Kill the old child process
            with suppress(OSError):
                old_child.terminate(force=True)

        # Capture any output
        result = output.getvalue()

        # Update the module-level child variable
        _child_process = namespace.get("child", _child_process)

        # Build response
        response = []
        if result:
            response.append(types.TextContent(type="text", text=result.rstrip()))
        else:
            response.append(
                types.TextContent(
                    type="text", text="Code executed successfully (no output)"
                )
            )

        status_info = f"Child process: {'active' if _child_process and _child_process.isalive() else 'inactive'}"
        response.append(types.TextContent(type="text", text=status_info))

    except Exception as e:  # noqa: BLE001
        # Update the module-level child variable even on error
        _child_process = namespace.get("child", _child_process)

        # Build error response
        response = []
        response.append(types.TextContent(type="text", text=f"Error: {e!s}"))

        output_text = output.getvalue()
        if output_text:
            response.append(
                types.TextContent(type="text", text=f"Output:\n{output_text}")
            )

        response.append(
            types.TextContent(type="text", text=f"Traceback:\n{traceback.format_exc()}")
        )

        status_info = f"Child process: {'active' if _child_process and _child_process.isalive() else 'inactive'}"
        response.append(types.TextContent(type="text", text=status_info))

    finally:
        # Update global child even on error
        new_child = namespace.get("child")
        if new_child is not old_child and old_child is not None:
            with suppress(Exception):
                old_child.terminate(force=True)
        _child_process = new_child

    return response


async def main() -> None:
    # Run the server using stdio transport
    async with mcp.server.stdio.stdio_server() as (read_stream, write_stream):
        await server.run(
            read_stream, write_stream, server.create_initialization_options()
        )


def run() -> None:
    """Entry point for console script."""
    asyncio.run(main())


if __name__ == "__main__":
    run()
