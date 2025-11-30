# dbg-cli

LLM-optimized debugger interface for LLDB and RR.

## Overview

`dbg-cli` provides a Python-based interface for debuggers optimized for LLM agents. LLMs write Python code to stdin, which is executed in a namespace with the `dbg` debugger object available.

Key features:
- **Python code input** - LLMs write natural Python code, not debugger commands
- **JSON responses** - All output is structured JSON with full state
- **Persistent sessions** - Managed via pueue for long-running debugging
- **Reverse debugging** - RR backend supports time-travel debugging

## Quick Start

```bash
# Start a new debugging session
dbg-cli --start --backend lldb
# Output: {"session_id": "a1b2c3d4", "backend": "lldb"}

# Send Python code to control the debugger
echo 'dbg.launch("/path/to/binary", ["arg1", "arg2"])' | dbg-cli a1b2c3d4
echo 'dbg.breakpoint("main")' | dbg-cli a1b2c3d4
echo 'dbg.continue_()' | dbg-cli a1b2c3d4
echo 'dbg.backtrace()' | dbg-cli a1b2c3d4
echo 'dbg.locals()' | dbg-cli a1b2c3d4

# Multi-line code
dbg-cli a1b2c3d4 << 'EOF'
result = dbg.backtrace()
for frame in result["result"]["frames"]:
    print(f"{frame['index']}: {frame['function']}")
EOF

# List active sessions
dbg-cli --list

# Stop a session
dbg-cli --stop a1b2c3d4
```

## Python API (dbg object)

The `dbg` object is available in the execution namespace with these methods:

### Session Management
```python
dbg.launch(binary, args=[])    # Start debugging a program
dbg.attach(pid)                # Attach to running process
dbg.detach()                   # Detach from process
dbg.quit()                     # End debugging session
```

### Execution Control
```python
dbg.continue_()                # Continue execution (note: underscore)
dbg.step(count=1)              # Step into (source level)
dbg.next(count=1)              # Step over (source level)
dbg.finish()                   # Run until function returns

# Reverse debugging (RR only)
dbg.reverse_continue()         # Run backwards
dbg.reverse_step(count=1)      # Step backwards
dbg.reverse_next(count=1)      # Step over backwards
```

### Breakpoints
```python
dbg.breakpoint("main")         # Set breakpoint at function
dbg.breakpoint("file.c:42")    # Set breakpoint at file:line
dbg.breakpoint("0x1234")       # Set breakpoint at address
dbg.delete(bp_id)              # Delete breakpoint
dbg.breakpoints()              # List all breakpoints
dbg.enable(bp_id)              # Enable breakpoint
dbg.disable(bp_id)             # Disable breakpoint
dbg.watch("variable")          # Set watchpoint
```

### Inspection
```python
dbg.backtrace(count=None)      # Stack trace
dbg.frame(index)               # Select stack frame
dbg.thread(thread_id)          # Select thread
dbg.threads()                  # List all threads
dbg.locals()                   # Local variables
dbg.args()                     # Function arguments
dbg.print("expression")        # Evaluate expression
dbg.memory("0x1234", size=64)  # Read memory
dbg.registers()                # CPU registers
```

### Source
```python
dbg.source(location=None)      # Get source code
dbg.disassemble(location=None) # Get disassembly
dbg.status()                   # Get current status
```

### Short Aliases
All methods have short aliases: `c`, `s`, `n`, `b`, `bt`, `f`, `p`, `x`, `l`, `dis`, etc.

## Response Format

All responses are JSON:

```json
{
  "status": "ok",
  "output": "any print() output",
  "result": {"frames": [...]},
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
  }
}
```

On error:
```json
{
  "status": "error",
  "error": "Error message",
  "traceback": "Python traceback...",
  "state": {...}
}
```

## Backends

### LLDB (default)
Uses LLDB Python bindings.

```bash
dbg-cli --start --backend lldb
```

### RR (Record and Replay)
Supports reverse debugging with recorded traces.

```bash
# First, record a trace
rr record /path/to/binary

# Then replay with dbg-cli
dbg-cli --start --backend rr
echo 'dbg.launch("~/.local/share/rr/binary-0")' | dbg-cli SESSION_ID
echo 'dbg.reverse_continue()' | dbg-cli SESSION_ID  # Time travel!
```

## Architecture

```
                    ┌─────────────────┐
   LLM Agent ──────►│    dbg-cli      │ (client)
   (Python code)    │   stdin/stdout  │
                    └────────┬────────┘
                             │ Unix Socket (JSON)
                    ┌────────▼────────┐
                    │   dbg-server    │ (managed by pueue)
                    │   exec(code)    │
                    │   namespace:    │
                    │     dbg = ...   │
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
    (pkgs.callPackage ./pkgs/dbg-cli { })
  ];
}
```
