# pexpect-cli

A CLI tool for managing persistent pexpect sessions using pueue as the process
manager.

## Features

- **Persistent sessions**: Maintain long-running interactive processes across
  multiple invocations
- **Multiple sessions**: Run multiple pexpect sessions in parallel
- **Built-in monitoring**: Leverage pueue's logging and status tracking
- **Real-time output**: Stream output to pueue logs as commands execute
- **Isolated queue**: All sessions run in a dedicated `pexpect` group
- **Simple interface**: Execute Python code via stdin
- **XDG compliant**: Stores sockets in XDG_RUNTIME_DIR with secure fallback

## Architecture

- **pexpect-server**: Long-running server managed by pueue that maintains the
  pexpect namespace
- **pexpect-cli**: Client that sends Python code to the server via Unix sockets
- **Session ID**: Unique UUID-based identifiers (not pueue task IDs)
- **Pueue group**: All sessions run in the `pexpect` group for isolation
- **Logging**: Server streams output to stdout in real-time, captured by pueue
- **Streaming**: Output is both displayed in real-time and returned to client

## Installation

With Nix:

```bash
nix run github:Mic92/dotfiles#pexpect-cli
```

Or add to your NixOS/home-manager configuration.

## Usage

### Start a new session

```bash
$ pexpect-cli --start
5901c22d
```

The session ID is a unique 8-character hex string. Optionally add a label:

```bash
$ pexpect-cli --start --name ssh-prod
a3f4b2c1
```

### Execute code in a session

Code is read from stdin:

```bash
# Using echo
$ echo 'child = pexpect.spawn("bash")' | pexpect-cli 5901c22d

# Using heredoc
$ pexpect-cli 5901c22d <<EOF
child.sendline('pwd')
child.expect('\$')
print(child.before.decode())
EOF

# From a file
$ pexpect-cli 5901c22d < script.py
```

### Monitor sessions

All sessions run in the `pexpect` group for easy isolation:

```bash
# View all pexpect sessions
$ pueue status --group pexpect

# Follow live output in real-time
$ pueue follow <task-id>

# View full logs
$ pueue log <task-id>

# List sessions with pexpect-cli
$ pexpect-cli --list
5901c22d: Running
a3f4b2c1: Running (ssh-prod)
```

**Finding task IDs**: When you start a session, note the task ID from
`pueue status --group pexpect`. The task ID is shown in the first column
(different from session ID).

### Stop a session

```bash
$ pexpect-cli --stop 5901c22d

# Or use pueue directly with the task ID
$ pueue kill <task-id>
```

### One-shot execution (no persistence)

If no session ID is provided, code runs in a temporary namespace:

```bash
$ echo 'print(pexpect.spawn("echo hello").read().decode())' | pexpect-cli
hello
```

## Examples

### Interactive SSH session

```bash
# Start session
$ session=$(pexpect-cli --start --name ssh-session)

# Connect to SSH
$ pexpect-cli $session <<'EOF'
child = pexpect.spawn('ssh user@example.com')
child.expect('password:')
child.sendline('mypassword')
child.expect('\$')
print("Connected!")
EOF

# Run commands
$ pexpect-cli $session <<'EOF'
child.sendline('uptime')
child.expect('\$')
print(child.before.decode())
EOF

# Cleanup
$ pexpect-cli --stop $session
```

### Database interaction

```bash
$ session=$(pexpect-cli --start --name db-session)

$ pexpect-cli $session <<'EOF'
child = pexpect.spawn('sqlite3 mydb.db')
child.expect('sqlite>')
child.sendline('.tables')
child.expect('sqlite>')
print("Tables:", child.before.decode())
EOF
```

### Automating vim

```bash
$ pexpect-cli --start | read session

$ pexpect-cli $session <<'EOF'
child = pexpect.spawn('vim test.txt')
child.expect('.')
child.send('i')  # Insert mode
child.send('Hello from pexpect!')
child.send('\x1b')  # ESC
child.send(':wq\r')  # Save and quit
child.expect(pexpect.EOF)
print("File created")
EOF
```

## Available in Namespace

When executing code, the following are available:

- `pexpect`: The pexpect module
- `child`: Persistent child process variable (initially None, persists across
  executions in the same session)

## Socket Location

Sockets are stored securely with proper permissions (0o700):

- **Preferred**: `$XDG_RUNTIME_DIR/pexpect-cli/{session_id}.sock` (tmpfs,
  auto-cleanup on logout)
- **Fallback**: `$XDG_CACHE_HOME/pexpect-cli/sockets/{session_id}.sock` or
  `~/.cache/pexpect-cli/sockets/{session_id}.sock`

## Advanced Usage

### Leverage pueue features

Since sessions are pueue tasks in the `pexpect` group, you can use pueue
features:

```bash
# View only pexpect sessions
$ pueue status --group pexpect

# Kill all pexpect sessions
$ pueue clean --group pexpect

# Pause/start the pexpect group
$ pueue pause --group pexpect
$ pueue start --group pexpect
```

Note: Sessions are automatically added to the `pexpect` group - you don't need
to manage this manually.

### Multiple parallel sessions

```bash
# Start 5 sessions in parallel
$ for i in {1..5}; do
    pexpect-cli --start --name "worker-$i"
  done

# Execute in all
$ pexpect-cli --list | awk '{print $1}' | while read -r id; do
    echo 'print("hello from", '$id')' | pexpect-cli "$id"
  done
```

## Comparison with MCP Version

The original MCP-based implementation was designed for Claude Code integration.
This CLI version:

- ✅ **Simpler**: ~220 lines vs 340 lines
- ✅ **No MCP dependency**: Standalone tool
- ✅ **Better monitoring**: Native pueue integration
- ✅ **More flexible**: Can be used in scripts, pipelines, etc.
- ✅ **Cleaner logging**: Uses pueue's logging, no custom log files
- ✅ **Process management**: Pueue handles all daemon complexity

## Troubleshooting

### Session not starting

```bash
# Check pueue daemon is running
$ pueue status

# If not, start it
$ pueued -d
```

### Socket not found

```bash
# Check session is actually running
$ pueue status

# Check socket location
$ ls -la $XDG_RUNTIME_DIR/pexpect-cli/
# or
$ ls -la ~/.cache/pexpect-cli/sockets/
```

### View detailed server logs

```bash
# Follow server output
$ pueue follow <session-id>

# View full log
$ pueue log <session-id>
```

## License

This project follows the same license as the parent repository.
