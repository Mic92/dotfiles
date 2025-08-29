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

## Usage with Claude Code

### Adding to Claude Code

To use pexpect-mcp with Claude Code, simply run:

```bash
claude mcp add pexpect-mcp nix run github:Mic92/dotfiles#pexpect-mcp
```

This will automatically configure the MCP server in your Claude Code settings.

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

### Tool: run_pexpect

Executes Python code with the pexpect library available.

**Parameters:**

- `code` (string, required): Python code to execute
- `timeout` (number, optional): Execution timeout in seconds (minimum 30s)

**Available in scope:**

- `pexpect`: The pexpect module
- `child`: Persistent child process variable (initially None)

### Response Format

The tool returns:

- **On success**: Output from `print()` statements, child process status, and
  log file location
- **On failure**: Error message, any partial output, full traceback, and log
  file location

The logs include colored output with:

- Bold blue headers showing execution context
- Syntax-highlighted Python code (with dark background for readability)
- Bold green success or bold red error messages
- All `print()` output and stdout captured during execution

## Advanced Usage

### Persistent Child Process

The `child` variable persists across multiple tool calls, allowing you to
maintain long-running interactive sessions:

```python
# First call - start a session
child = pexpect.spawn('bash')
child.sendline('cd /tmp')
child.expect('\\$')
print("Changed to /tmp directory")
```

```python
# Second call - child is still in /tmp
if child and child.isalive():
    child.sendline('pwd')
    child.expect('\\$')
    print(f"Current dir: {child.before.decode()}")
```

```python
# Third call - clean up
if child and child.isalive():
    child.sendline('exit')
    child.expect(pexpect.EOF)
    child = None  # Explicitly clear the child
    print("Session closed")
```

### Practical Examples

#### Automating SSH Sessions

```python
child = pexpect.spawn('ssh user@example.com')
index = child.expect(['password:', 'continue connecting', pexpect.TIMEOUT])
if index == 1:  # First time connecting
    child.sendline('yes')
    child.expect('password:')
child.sendline('mypassword')
child.expect('\\$')
print("Connected via SSH")
```

#### Interactive Database Operations

```python
child = pexpect.spawn('sqlite3 mydb.db')
child.expect('sqlite>')
child.sendline('.tables')
child.expect('sqlite>')
tables = child.before.decode()
print(f"Database tables: {tables}")
```

#### Automating Terminal Applications

```python
# Automate vim to create and edit a file
child = pexpect.spawn('vim test.txt')
child.expect('.')  # Wait for vim to load
child.send('i')  # Enter insert mode
child.send('Hello from pexpect!')
child.send('\x1b')  # ESC key
child.send(':wq\r')  # Save and quit
child.expect(pexpect.EOF)
print("File created with vim")
```
