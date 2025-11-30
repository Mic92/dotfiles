# debugger-cli

LLM-optimized debugger interface for LLDB and RR.

## Overview

`debugger-cli` provides a clean, JSON-based interface for debuggers that is optimized for use by LLM agents. Every response includes:

- Full debugger state (threads, frames, source context)
- Structured error information with typed error codes
- Command-specific results

## Quick Start

```bash
# Start a new debugging session
debugger-cli --start --backend lldb
# Output: {"session_id": "a1b2c3d4", "backend": "lldb"}

# Send commands (JSON responses)
echo "launch /path/to/binary arg1 arg2" | debugger-cli a1b2c3d4
echo "break main" | debugger-cli a1b2c3d4
echo "continue" | debugger-cli a1b2c3d4
echo "backtrace" | debugger-cli a1b2c3d4
echo "print variable_name" | debugger-cli a1b2c3d4

# List active sessions
debugger-cli --list

# Stop a session
debugger-cli --stop a1b2c3d4
```

## Commands

### Session Management
- `launch <binary> [args...]` - Start debugging a program
- `attach <pid>` - Attach to a running process
- `detach` - Detach from current process
- `quit` / `exit` - End debugging session

### Execution Control
- `continue` / `c` - Continue execution
- `step [count]` / `s` - Step into (source level)
- `next [count]` / `n` - Step over (source level)
- `finish` / `fin` - Run until current function returns
- `reverse-continue` / `rc` - Reverse continue (RR only)
- `reverse-step` / `rs` - Reverse step (RR only)
- `reverse-next` / `rn` - Reverse next (RR only)

### Breakpoints
- `break <location>` / `b` - Set breakpoint (file:line, function, address)
- `delete <id>` / `d` - Delete breakpoint
- `breakpoints` / `bl` - List breakpoints
- `enable <id>` - Enable breakpoint
- `disable <id>` - Disable breakpoint
- `watch <expression>` - Set watchpoint

### Inspection
- `backtrace [count]` / `bt` - Stack trace
- `frame <n>` / `f` - Select frame
- `thread <id>` / `t` - Select thread
- `threads` - List threads
- `locals` / `lo` - Local variables
- `args` - Function arguments
- `print <expr>` / `p` - Evaluate expression
- `memory <addr> [size]` / `x` - Read memory
- `registers` / `reg` - CPU registers

### Source
- `list [location]` / `l` - Source code
- `disassemble [location]` / `dis` - Disassembly

## Response Format

All responses are JSON with this structure:

```json
{
  "status": "ok|stopped|error",
  "state": {
    "running": false,
    "pid": 12345,
    "executable": "/path/to/binary",
    "threads": [...],
    "current_thread_id": 1,
    "current_frame": {...},
    "source_context": [...],
    "breakpoints": [...],
    "backend": "lldb"
  },
  "stop_reason": "breakpoint|step|signal|exited|none",
  "result": {...},
  "error": {
    "type": "invalid_command|parse_error|debugger_error|...",
    "message": "Human-readable message",
    "details": {...}
  }
}
```

## Backends

### LLDB
The default backend. Uses LLDB Python bindings for debugger control.

```bash
debugger-cli --start --backend lldb
```

### RR (Record and Replay)
Supports reverse debugging with recorded traces.

```bash
# Record a trace first
rr record /path/to/binary

# Then replay
debugger-cli --start --backend rr
echo "launch ~/.local/share/rr/binary-0" | debugger-cli SESSION_ID
```

## Architecture

```
                    ┌─────────────────┐
   LLM Agent ──────►│  debugger-cli   │ (client)
                    │   stdin/stdout  │
                    └────────┬────────┘
                             │ Unix Socket
                    ┌────────▼────────┐
                    │ debugger-server │ (managed by pueue)
                    │   StateManager  │
                    └────────┬────────┘
                             │
              ┌──────────────┴──────────────┐
              ▼                              ▼
       ┌─────────────┐                ┌─────────────┐
       │ LLDBBackend │                │  RRBackend  │
       │ (Python API)│                │  (GDB/MI)   │
       └─────────────┘                └─────────────┘
```

## Requirements

- Python 3.11+
- pueue (process management)
- LLDB with Python bindings (for LLDB backend)
- RR (for RR backend, optional)

## Installation (Nix)

```nix
{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.callPackage ./pkgs/debugger-cli { })
  ];
}
```
