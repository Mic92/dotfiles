# pexpect-mcp

An MCP (Model Context Protocol) server that provides access to the pexpect
library for managing interactive processes.

## Features

- Single persistent `child` process variable that persists across tool calls
- Automatic process cleanup when `child` is reassigned
- Python code execution with pexpect library in scope
- Optional execution timeout (minimum 30 seconds)
- Comprehensive logging with live monitoring capability

## Requirements

- Python 3.13+ support

## Installation

The package is available as a Nix flake:

```bash
nix run .#pexpect-mcp
```

## Usage

The server provides one tool: `run_pexpect`

### Monitoring Execution Logs

All pexpect executions are logged to `~/.cache/pexpect-mcp/<project-path>.log`.
You can monitor these logs in real-time using:

```bash
pexpect-mcp --follow
```

This will tail the log file for the current working directory, showing all
output from `print()` statements and stdout redirections. The log file is
replaced atomically on each execution, and `--follow` will automatically detect
and handle these replacements.

#### Color Themes

The log output includes syntax highlighting and colored status messages. You can
configure the color scheme using the `PEXPECT_MCP_COLORS` environment variable:

```bash
# Dark terminal theme (default) - bright colors
export PEXPECT_MCP_COLORS=dark

# Light terminal theme (e.g., Solarized Light) - darker colors
export PEXPECT_MCP_COLORS=light

# Disable colors entirely
export PEXPECT_MCP_COLORS=none
```

The light theme uses darker colors that are more readable on light backgrounds,
while the dark theme uses bright colors optimized for dark terminals.

### Tool: run_pexpect

Executes Python code with the pexpect library available.

**Parameters:**

- `code` (string, required): Python code to execute
- `timeout` (number, optional): Execution timeout in seconds (minimum 30s)

**Available in scope:**

- `pexpect`: The pexpect module
- `child`: Persistent child process variable (initially None)

### Example Usage

```python
# Create a child process
child = pexpect.spawn('bash')
child.sendline('echo Hello World')
child.expect('Hello World')
print(child.before.decode())

# In the next call, child is still available
if child and child.isalive():
    child.sendline('exit')
    child.expect(pexpect.EOF)

# Creating a new child automatically kills the old one
child = pexpect.spawn('python3')  # Old bash process is terminated
```

### Response Format

The tool returns a dictionary with:

- `success` (bool): Whether execution succeeded
- `output` (string): Captured stdout
- `last_value` (string|null): String representation of the last expression value
- `child_active` (bool): Whether a child process is currently alive
- `error` (string, on failure): Error message
- `traceback` (string, on failure): Full traceback

## Development

```bash
# Run tests
nix develop .#pexpect-mcp -c python3 test_server.py
```
