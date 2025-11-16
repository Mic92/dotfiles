# strace-darwin: System Call Tracer for macOS using LLDB

## Overview

Implement a strace-compatible system call tracer for macOS (Darwin) using LLDB Python API as the tracing backend. This tool will provide Linux strace-like functionality on macOS where dtruss/DTrace is blocked by System Integrity Protection (SIP).

## Project Metadata

- **License**: MIT
- **Language**: Python 3
- **Platform**: macOS ARM64 (Apple Silicon)
- **Location**: `pkgs/strace-darwin/` in Mic92/dotfiles repository
- **Target**: All implementation phases (MVP → Enhanced → Process Management → Advanced)
- **Quality**: Test-driven development from the start

## Technical Architecture

### Core Approach

Use **LLDB Python API** to set breakpoints on syscall wrapper functions in `libsystem_kernel`, avoiding SIP restrictions that block dtruss/DTrace.

```
User Command → strace-darwin (Python) → LLDB Python API → liblldb.so → debugserver → Mach APIs
```

### Why LLDB?

1. ✅ **SIP-compatible**: Can attach to user processes without disabling SIP
2. ✅ **Stable API**: SB* classes are the official stable C++ API exposed to Python via SWIG
3. ✅ **Mach-based**: Uses proper macOS debugging APIs (task_for_pid, thread_get_state, vm_read)
4. ✅ **Architecture abstraction**: Handles ARM64/x86_64 differences internally
5. ✅ **Available**: Ships with Xcode Command Line Tools (system LLDB)

### Architecture Details

**Platform**: macOS ARM64 (Apple Silicon)
- Syscall instruction: `SVC 0x80`
- Syscall number register: `x16`
- Argument registers: `x0-x7`
- Return value register: `x0`
- No `SYSCALL_CLASS_MASK` (unlike x86_64)

**Syscall Interception Strategy**:
1. Hook syscall wrapper functions in `libsystem_kernel` (e.g., `__open`, `__read`, `__write`)
2. Set LLDB breakpoints on these wrappers
3. Extract syscall number from `x16` register
4. Parse arguments from `x0-x7` registers and memory
5. Continue execution and capture return value
6. Format output to match strace conventions

## Requirements

### Functional Requirements

#### FR1: Syscall Coverage
- **MUST**: Support **all** macOS syscalls (200+ syscalls)
- **MUST**: Provide detailed pretty-printing for **important syscalls**:
  - File operations: `open`, `openat`, `read`, `write`, `close`, `stat`, `fstat`, `lstat`, `access`
  - Memory: `mmap`, `munmap`, `mprotect`, `brk`
  - Process: `fork`, `vfork`, `execve`, `exit`, `wait4`, `getpid`
  - Network: `socket`, `connect`, `bind`, `listen`, `accept`, `send`, `recv`
  - I/O multiplexing: `select`, `poll`, `kqueue`, `kevent`
  - Signals: `signal`, `sigaction`, `kill`, `sigprocmask`
- **MAY**: Provide basic pretty-printing for less common syscalls (hex dump arguments)

#### FR2: CLI Compatibility
- **MUST**: Be mostly compatible with Linux strace for user-facing options
- **MUST**: Support these strace options in MVP:
  - `-c` : Summary statistics (count, time, errors per syscall)
  - `-e trace=<set>` : Filter syscalls (e.g., `trace=open,close` or `trace=file`)
  - `-o <file>` : Write output to file
  - `-p <pid>` : Attach to existing process
  - `-t` : Print timestamp on each line
  - `-T` : Show time spent in each syscall
  - `-yy` : Print protocol-specific information for file descriptors
- **SHOULD**: Support these options in later phases:
  - `-f` : Follow forks
  - `-ff` : Follow forks with separate files
  - `-s <size>` : Max string size to print
  - `-v` : Verbose mode (full structure decoding)
  - `-x` / `-xx` : Print strings in hex
  - `-r` : Print relative timestamps
- **MAY**: Differ from strace where macOS platform demands it (document differences)

#### FR3: Output Format
- **MUST**: Match strace output format for common cases:
  ```
  open("/etc/hosts", O_RDONLY) = 3
  fstat(3, {st_mode=S_IFREG|0644, st_size=450, ...}) = 0
  close(3) = 0
  ```
- **MUST**: Use stderr for syscall output (like strace)
- **MAY**: Use Darwin-specific formatting where appropriate (e.g., BSD kqueue vs Linux epoll)

#### FR4: Process Attachment
- **MUST**: Launch and trace new processes: `strace-darwin /bin/ls`
- **MUST**: Attach to running processes: `strace-darwin -p 1234`
- **MUST**: Handle process lifecycle (startup, exit, crashes)
- **SHOULD**: Follow child processes with `-f` flag

#### FR5: Error Handling
- **MUST**: Detect and report when LLDB cannot attach (permissions, hardened runtime, etc.)
- **MUST**: Gracefully handle target process crashes
- **MUST**: Clean up breakpoints and detach on Ctrl+C
- **MUST NOT**: Support tracing SIP-protected system binaries (out of scope)

### Non-Functional Requirements

#### NFR1: Performance
- **SHOULD**: Overhead < 5x native execution for typical workloads
- **MUST**: Not crash or hang on high-frequency syscalls
- **SHOULD**: Support buffered output to reduce overhead

#### NFR2: Compatibility
- **MUST**: Work with system LLDB (from Xcode Command Line Tools)
- **MUST**: Support macOS 12+ (Monterey and later)
- **MUST**: Work with ARM64 architecture
- **NICE TO HAVE**: x86_64 support (Intel Macs) in future

#### NFR3: Code Quality
- **MUST**: Follow test-driven development
- **MUST**: Include unit tests for syscall parsing
- **MUST**: Include integration tests for common programs
- **MUST**: Use type hints (mypy-compatible)
- **MUST**: Follow repository's treefmt formatting standards

#### NFR4: Documentation
- **MUST**: Include README.md with:
  - Installation instructions
  - Usage examples
  - Known limitations (SIP binaries, hardened runtime apps)
  - Comparison with Linux strace
- **SHOULD**: Include architecture documentation
- **SHOULD**: Document syscall signature database format

## Implementation Phases

### Phase 1: MVP - Basic Tracing (Weeks 1-2)

**Goal**: Prove the concept works with basic syscall tracing

**Deliverables**:
- [ ] LLDB Python wrapper that can attach to processes
- [ ] Breakpoint-based syscall interception
- [ ] Support 20 common syscalls with detailed pretty-printing:
  - File: `open`, `openat`, `close`, `read`, `write`, `stat`, `fstat`, `lstat`
  - Process: `execve`, `fork`, `exit`, `getpid`, `getppid`
  - Memory: `mmap`, `munmap`, `brk`
  - Network: `socket`, `connect`, `bind`
  - Misc: `ioctl`, `fcntl`
- [ ] Basic argument parsing (integers, strings, pointers)
- [ ] Strace-like output format
- [ ] `-o <file>` output redirection
- [ ] Unit tests for syscall argument parsing
- [ ] Integration tests with simple programs (`ls`, `cat`, `echo`)
- [ ] Nix package definition
- [ ] Basic README

**Success Criteria**:
```bash
$ strace-darwin ls /tmp
open("/tmp", O_RDONLY|O_DIRECTORY) = 3
fstat(3, {st_mode=S_IFDIR|0755, ...}) = 0
getdirentries(3, ...) = 512
close(3) = 0
exit(0) = ?
```

### Phase 2: Enhanced Features (Weeks 3-4)

**Goal**: Full syscall coverage and essential strace options

**Deliverables**:
- [ ] Support **all** macOS ARM64 syscalls (200+)
- [ ] Syscall signature database (name, args, return type)
- [ ] Enhanced argument parsing:
  - Structures: `struct stat`, `struct sockaddr`, `struct timespec`
  - Arrays: `char *argv[]`, `struct iovec[]`
  - Flags: `O_RDONLY|O_CREAT`, `PROT_READ|PROT_WRITE`
  - Pointers: Dereference and display pointed-to data
- [ ] `-yy` socket/fd decoding (show socket addresses, file paths)
- [ ] `-t` and `-T` timing options
- [ ] `-c` summary statistics
- [ ] `-e trace=<set>` filtering (both specific syscalls and classes)
- [ ] `-p <pid>` attach to running process
- [ ] Improved error messages and edge case handling
- [ ] Comprehensive test suite (50+ tests)
- [ ] Performance benchmarking

**Success Criteria**:
```bash
$ strace-darwin -c sleep 1
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 99.99    0.000050          50         1           nanosleep
  0.01    0.000000           0         5           mmap
  0.00    0.000000           0         1           exit
------ ----------- ----------- --------- --------- ----------------
100.00    0.000050                     7           total

$ strace-darwin -yy nc localhost 8080
socket(AF_INET, SOCK_STREAM, IPPROTO_TCP) = 3
connect(3, {sa_family=AF_INET, sin_port=8080, sin_addr=127.0.0.1}, 16) = 0
...
```

### Phase 3: Process Management (Weeks 5-6)

**Goal**: Multi-process tracing

**Deliverables**:
- [ ] `-f` follow forks and child processes
- [ ] `-ff` separate output files per process
- [ ] Thread awareness (distinguish threads in output)
- [ ] Signal tracing and reporting
- [ ] Process tree tracking
- [ ] Coordinated output from multiple processes
- [ ] Handle complex fork/exec scenarios
- [ ] Tests for multi-process programs

**Success Criteria**:
```bash
$ strace-darwin -f sh -c "ls | grep tmp"
[pid 1234] execve("/bin/sh", ["sh", "-c", "ls | grep tmp"], ...) = 0
[pid 1234] pipe([3, 4]) = 0
[pid 1234] fork() = 1235
[pid 1235] dup2(4, 1) = 1
[pid 1235] execve("/bin/ls", ["ls"], ...) = 0
[pid 1234] fork() = 1236
[pid 1236] dup2(3, 0) = 0
[pid 1236] execve("/usr/bin/grep", ["grep", "tmp"], ...) = 0
...
```

### Phase 4: Advanced Features (Week 7+)

**Goal**: Polish and advanced capabilities

**Deliverables**:
- [ ] `-e read=<set>` / `-e write=<set>` data dumping
- [ ] Advanced filtering: syscall classes (`-e trace=file`, `-e trace=network`)
- [ ] `-v` verbose mode (full structure decoding)
- [ ] `-x` / `-xx` hex output modes
- [ ] `-r` relative timestamps
- [ ] Performance optimization (conditional breakpoints, batched logging)
- [ ] Comprehensive documentation
- [ ] Man page
- [ ] Comparison guide: strace vs strace-darwin

## File Structure

```
pkgs/strace-darwin/
├── default.nix                    # Nix package definition
├── pyproject.toml                 # Python package metadata
├── README.md                      # User documentation
├── ARCHITECTURE.md                # Internal architecture docs
├── LICENSE                        # MIT license
├── strace_darwin/
│   ├── __init__.py
│   ├── __main__.py                # Entry point (argument parsing)
│   ├── tracer.py                  # Core LLDB tracer implementation
│   ├── syscalls/
│   │   ├── __init__.py
│   │   ├── registry.py            # Syscall database/registry
│   │   ├── arm64.py               # ARM64 syscall numbers and signatures
│   │   ├── parsers.py             # Argument parsers (int, string, struct, etc.)
│   │   └── formatters.py          # Output formatters (strace-like output)
│   ├── breakpoints.py             # Breakpoint management
│   ├── process.py                 # Process lifecycle management
│   └── statistics.py              # Statistics aggregation for -c
├── tests/
│   ├── __init__.py
│   ├── test_syscall_parsing.py   # Unit tests for argument parsing
│   ├── test_formatters.py         # Unit tests for output formatting
│   ├── test_integration.py        # Integration tests
│   └── fixtures/
│       ├── simple_program.c       # Test programs
│       └── fork_exec.c
└── examples/
    └── usage_examples.md
```

## Nix Integration

### Package Definition (`pkgs/strace-darwin/default.nix`)

```nix
{ lib
, python3Packages
, lldb
, stdenv
}:

python3Packages.buildPythonApplication {
  pname = "strace-darwin";
  version = "0.1.0";

  src = ./.;
  format = "pyproject";

  nativeBuildInputs = with python3Packages; [
    setuptools
    wheel
  ];

  propagatedBuildInputs = [
    lldb  # System LLDB with Python bindings
  ];

  checkInputs = with python3Packages; [
    pytest
    pytest-timeout
  ];

  # Run tests during build
  checkPhase = ''
    pytest tests/
  '';

  meta = with lib; {
    description = "System call tracer for macOS using LLDB";
    homepage = "https://github.com/Mic92/dotfiles";
    license = licenses.mit;
    platforms = platforms.darwin;
    maintainers = [ maintainers.mic92 ];
  };
}
```

### Registration (`pkgs/flake-module.nix`)

```nix
# Add to Darwin-only packages (around line 31-34)
// pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
  blueutil = pkgs.callPackage ./blueutil { };
  systemctl-macos = pkgs.callPackage ./systemctl { };
  strace-darwin = pkgs.callPackage ./strace-darwin { };  # ADD THIS
}
```

### System Integration

Option 1: Machine-level (`machines/evo/configuration.nix`):
```nix
environment.systemPackages = [
  self.packages.${pkgs.stdenv.hostPlatform.system}.strace-darwin
];

# Optional: Create strace alias
environment.shellAliases = {
  strace = "strace-darwin -yy";  # Match Linux convention
};
```

Option 2: Home-manager (`home-manager/macos.nix`):
```nix
home.packages = [
  pkgs.strace-darwin
];
```

## Core Implementation Details

### 1. LLDB Tracer (`strace_darwin/tracer.py`)

```python
import lldb
from typing import Dict, List, Optional
from .syscalls.registry import SyscallRegistry
from .breakpoints import BreakpointManager

class LLDBTracer:
    def __init__(self, args):
        self.debugger = lldb.SBDebugger.Create()
        self.target = None
        self.process = None
        self.syscall_registry = SyscallRegistry()
        self.breakpoint_mgr = BreakpointManager(self.syscall_registry)
        self.args = args

    def attach_to_process(self, pid: int) -> bool:
        """Attach to existing process by PID"""
        # Get target for the process
        error = lldb.SBError()
        self.target = self.debugger.CreateTarget("")

        # Attach using debugserver
        attach_info = lldb.SBAttachInfo(pid)
        self.process = self.target.Attach(attach_info, error)

        if not error.Success():
            print(f"Failed to attach: {error.GetCString()}")
            return False

        # Set up breakpoints on syscall wrappers
        self.breakpoint_mgr.setup_breakpoints(self.target)
        return True

    def launch_process(self, executable: str, args: List[str]) -> bool:
        """Launch new process and trace it"""
        error = lldb.SBError()
        self.target = self.debugger.CreateTarget(executable, None, None, True, error)

        if not error.Success():
            print(f"Failed to create target: {error.GetCString()}")
            return False

        # Set up breakpoints before launching
        self.breakpoint_mgr.setup_breakpoints(self.target)

        # Launch process
        launch_info = lldb.SBLaunchInfo(args)
        launch_info.SetWorkingDirectory(os.getcwd())

        self.process = self.target.Launch(launch_info, error)

        if not error.Success():
            print(f"Failed to launch: {error.GetCString()}")
            return False

        return True

    def syscall_callback(self, frame: lldb.SBFrame, bp_loc: lldb.SBBreakpointLocation) -> bool:
        """Called when syscall breakpoint is hit"""
        thread = frame.GetThread()

        # Extract syscall number from x16 register (ARM64)
        x16 = frame.FindRegister("x16")
        syscall_num = x16.GetValueAsUnsigned()

        # Look up syscall info
        syscall_info = self.syscall_registry.get_syscall(syscall_num)

        if not syscall_info:
            print(f"Unknown syscall: {syscall_num}", file=sys.stderr)
            return True  # Continue execution

        # Extract arguments from x0-x7 registers
        args = []
        for i in range(syscall_info.num_args):
            reg = frame.FindRegister(f"x{i}")
            args.append(reg.GetValueAsUnsigned())

        # Parse and format arguments
        formatted_args = self.syscall_registry.format_args(syscall_info, args, frame)

        # Print syscall entry
        print(f"{syscall_info.name}({formatted_args}) = ", end="", file=sys.stderr)

        # Set up return breakpoint to capture return value
        # TODO: Implement return value capture

        return True  # Continue execution

    def run(self) -> int:
        """Main event loop"""
        if self.args.attach:
            if not self.attach_to_process(self.args.attach):
                return 1
        else:
            if not self.launch_process(self.args.command[0], self.args.command):
                return 1

        # Event loop
        listener = self.debugger.GetListener()
        event = lldb.SBEvent()

        while True:
            if listener.WaitForEvent(1, event):
                if lldb.SBProcess.EventIsProcessEvent(event):
                    state = lldb.SBProcess.GetStateFromEvent(event)

                    if state == lldb.eStateExited:
                        exit_code = self.process.GetExitStatus()
                        print(f"exit({exit_code}) = ?", file=sys.stderr)
                        return exit_code

                    elif state == lldb.eStateStopped:
                        # Process stopped (probably at breakpoint)
                        continue

                    elif state == lldb.eStateCrashed:
                        print("Process crashed", file=sys.stderr)
                        return 1

        return 0
```

### 2. Syscall Registry (`strace_darwin/syscalls/registry.py`)

```python
from dataclasses import dataclass
from typing import List, Dict, Optional, Callable
from enum import Enum

class ArgType(Enum):
    INT = "int"
    UINT = "unsigned int"
    LONG = "long"
    ULONG = "unsigned long"
    PTR = "void*"
    STRING = "char*"
    STRUCT_STAT = "struct stat*"
    STRUCT_SOCKADDR = "struct sockaddr*"
    STRUCT_IOVEC = "struct iovec*"
    FLAGS = "flags"

@dataclass
class SyscallArg:
    name: str
    type: ArgType
    parser: Optional[Callable] = None

@dataclass
class SyscallInfo:
    number: int
    name: str
    args: List[SyscallArg]
    return_type: str

    @property
    def num_args(self) -> int:
        return len(self.args)

class SyscallRegistry:
    def __init__(self):
        self.syscalls: Dict[int, SyscallInfo] = {}
        self._register_arm64_syscalls()

    def _register_arm64_syscalls(self):
        """Register all ARM64 Darwin syscalls"""
        # File operations
        self.register(SyscallInfo(
            number=5,
            name="open",
            args=[
                SyscallArg("path", ArgType.STRING),
                SyscallArg("flags", ArgType.FLAGS),  # O_RDONLY, etc.
                SyscallArg("mode", ArgType.FLAGS),   # 0644, etc.
            ],
            return_type="int"
        ))

        self.register(SyscallInfo(
            number=3,
            name="read",
            args=[
                SyscallArg("fd", ArgType.INT),
                SyscallArg("buf", ArgType.PTR),
                SyscallArg("count", ArgType.ULONG),
            ],
            return_type="ssize_t"
        ))

        # Add all 200+ syscalls...
        # TODO: Complete syscall database

    def register(self, syscall: SyscallInfo):
        self.syscalls[syscall.number] = syscall

    def get_syscall(self, number: int) -> Optional[SyscallInfo]:
        return self.syscalls.get(number)

    def format_args(self, syscall: SyscallInfo, raw_args: List[int], frame: lldb.SBFrame) -> str:
        """Format arguments for display"""
        formatted = []

        for i, arg_spec in enumerate(syscall.args):
            raw_val = raw_args[i]

            if arg_spec.type == ArgType.STRING:
                # Read string from memory
                string_val = self._read_string(frame.GetProcess(), raw_val)
                formatted.append(f'"{string_val}"')

            elif arg_spec.type == ArgType.FLAGS:
                # Format as flags (O_RDONLY|O_CREAT)
                flag_str = self._format_flags(arg_spec.name, raw_val)
                formatted.append(flag_str)

            elif arg_spec.type == ArgType.INT:
                formatted.append(str(raw_val))

            elif arg_spec.type == ArgType.PTR:
                if raw_val == 0:
                    formatted.append("NULL")
                else:
                    formatted.append(f"0x{raw_val:x}")

            # TODO: Handle more types

        return ", ".join(formatted)

    def _read_string(self, process: lldb.SBProcess, addr: int, max_len: int = 256) -> str:
        """Read null-terminated string from process memory"""
        error = lldb.SBError()
        data = process.ReadMemory(addr, max_len, error)

        if not error.Success():
            return f"<unreadable: {error.GetCString()}>"

        # Find null terminator
        try:
            null_pos = data.index(b'\0')
            return data[:null_pos].decode('utf-8', errors='replace')
        except ValueError:
            return data.decode('utf-8', errors='replace') + '...'

    def _format_flags(self, flag_type: str, value: int) -> str:
        """Format flags as symbolic names (O_RDONLY|O_CREAT)"""
        # TODO: Implement flag formatting for open(), mmap(), etc.
        if flag_type == "flags" and value == 0:
            return "O_RDONLY"
        return f"0x{value:x}"
```

### 3. CLI Entry Point (`strace_darwin/__main__.py`)

```python
import argparse
import sys
from .tracer import LLDBTracer

def main():
    parser = argparse.ArgumentParser(
        description='System call tracer for Darwin/macOS',
        epilog='Similar to Linux strace, but using LLDB on macOS'
    )

    # Output options
    parser.add_argument('-o', '--output', type=str, help='Write output to file')
    parser.add_argument('-c', '--summary-only', action='store_true',
                        help='Count time, calls, and errors for each syscall')
    parser.add_argument('-e', '--expr', type=str,
                        help='Filter expression (e.g., trace=open,close)')

    # Timing options
    parser.add_argument('-t', '--absolute-timestamps', action='store_true',
                        help='Print absolute timestamps')
    parser.add_argument('-tt', '--timestamps-microseconds', action='store_true',
                        help='Print timestamps with microseconds')
    parser.add_argument('-T', '--syscall-times', action='store_true',
                        help='Show time spent in each syscall')
    parser.add_argument('-r', '--relative-timestamps', action='store_true',
                        help='Print relative timestamps')

    # Process options
    parser.add_argument('-p', '--attach', type=int, metavar='PID',
                        help='Attach to process with PID')
    parser.add_argument('-f', '--follow-forks', action='store_true',
                        help='Follow forks')
    parser.add_argument('-ff', '--follow-forks-separate', action='store_true',
                        help='Follow forks with separate output files')

    # Output formatting
    parser.add_argument('-yy', '--decode-fds-extended', action='store_true',
                        help='Print protocol-specific information for file descriptors')
    parser.add_argument('-s', '--string-limit', type=int, default=32,
                        help='Maximum string size to print (default: 32)')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Verbose mode')
    parser.add_argument('-x', '--strings-hex', action='store_true',
                        help='Print non-ASCII strings in hex')
    parser.add_argument('-xx', '--all-strings-hex', action='store_true',
                        help='Print all strings in hex')

    # Command to trace
    parser.add_argument('command', nargs='*', help='Command to trace')

    args = parser.parse_args()

    # Validation
    if not args.attach and not args.command:
        parser.error('Must specify either -p PID or command to trace')

    if args.attach and args.command:
        parser.error('Cannot specify both -p and command')

    # Redirect output if requested
    if args.output:
        sys.stderr = open(args.output, 'w')

    # Create and run tracer
    tracer = LLDBTracer(args)
    return tracer.run()

if __name__ == '__main__':
    sys.exit(main())
```

## Testing Requirements

### Unit Tests

```python
# tests/test_syscall_parsing.py
import pytest
from strace_darwin.syscalls.registry import SyscallRegistry, SyscallInfo, SyscallArg, ArgType

def test_syscall_registration():
    registry = SyscallRegistry()

    # Should have common syscalls
    open_syscall = registry.get_syscall(5)
    assert open_syscall is not None
    assert open_syscall.name == "open"
    assert len(open_syscall.args) == 3

def test_string_argument_formatting():
    registry = SyscallRegistry()
    # TODO: Mock LLDB frame and test string reading

def test_flag_formatting():
    registry = SyscallRegistry()
    # Test O_RDONLY | O_CREAT formatting
    # TODO: Implement
```

### Integration Tests

```python
# tests/test_integration.py
import subprocess
import pytest

def test_trace_simple_program():
    """Test tracing /bin/echo"""
    result = subprocess.run(
        ['strace-darwin', '/bin/echo', 'hello'],
        capture_output=True,
        text=True
    )

    assert result.returncode == 0
    assert 'write(' in result.stderr
    assert 'exit(' in result.stderr

def test_trace_with_output_file(tmp_path):
    """Test -o option"""
    output_file = tmp_path / "trace.out"

    result = subprocess.run(
        ['strace-darwin', '-o', str(output_file), '/bin/ls'],
        capture_output=True
    )

    assert result.returncode == 0
    assert output_file.exists()
    content = output_file.read_text()
    assert 'open(' in content or 'openat(' in content

@pytest.mark.slow
def test_attach_to_process():
    """Test -p option"""
    # Start a long-running process
    proc = subprocess.Popen(['sleep', '10'])

    try:
        result = subprocess.run(
            ['strace-darwin', '-p', str(proc.pid)],
            capture_output=True,
            timeout=2
        )
    except subprocess.TimeoutExpired:
        pass  # Expected
    finally:
        proc.kill()
        proc.wait()
```

## Known Limitations

Document these clearly in README.md:

1. **SIP-Protected Binaries**: Cannot trace system binaries in `/System/`, `/usr/bin/`, etc. without disabling SIP or code signature removal (out of scope)

2. **Hardened Runtime**: Apps with hardened runtime and no `get-task-allow` entitlement cannot be traced

3. **Performance**: LLDB-based tracing has higher overhead than kernel-level tracing (dtruss/DTrace)

4. **Syscall Coverage**: All syscalls are intercepted, but pretty-printing is limited to important ones

5. **Platform**: ARM64 only for MVP (x86_64 support possible in future)

## Acceptance Criteria

### Phase 1 MVP
- [ ] Can trace simple programs: `/bin/ls`, `/bin/cat`, `/bin/echo`
- [ ] Output matches strace format for common syscalls
- [ ] `-o` option works
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Nix package builds successfully
- [ ] Integrated into dotfiles flake
- [ ] README.md documents basic usage

### Phase 2 Enhanced
- [ ] All 200+ syscalls intercepted
- [ ] Important syscalls (50+) have pretty-printing
- [ ] `-c`, `-e`, `-p`, `-t`, `-T`, `-yy` options work
- [ ] Can attach to running processes
- [ ] Statistics output matches strace format
- [ ] Performance overhead < 5x for typical workloads
- [ ] Comprehensive test suite (50+ tests)

### Phase 3 Process Management
- [ ] Can trace programs that fork: `sh -c "ls | grep tmp"`
- [ ] `-f` follows child processes
- [ ] Output clearly distinguishes processes (PIDs)
- [ ] Handles complex fork/exec scenarios
- [ ] Thread-aware output

### Phase 4 Advanced
- [ ] All planned strace options implemented
- [ ] Performance optimizations in place
- [ ] Complete documentation (README, ARCHITECTURE, man page)
- [ ] Comparison guide with Linux strace
- [ ] Ready for potential extraction as standalone project

## Implementation Approach

### Development Workflow

1. **Test-Driven Development**:
   - Write failing test first
   - Implement minimum code to pass test
   - Refactor and improve
   - Repeat

2. **Iterative Phases**:
   - Complete Phase 1 MVP fully before moving to Phase 2
   - Each phase should be mergeable and usable
   - Document progress in commit messages

3. **Code Review Points**:
   - After each major feature
   - Before merging phases
   - Performance benchmarks

### Key Technical Decisions

1. **Syscall Database**: Use Python dictionaries for syscall registry (simple, extensible)
2. **Breakpoint Strategy**: Set breakpoints on `__syscall_name` wrappers in libsystem_kernel
3. **Output Format**: Match strace as closely as possible for user-facing features
4. **Error Handling**: Fail gracefully with helpful error messages
5. **Performance**: Start simple, optimize in Phase 4 based on profiling

## References

### LLDB Documentation
- LLDB Python API: https://lldb.llvm.org/python_api.html
- LLDB Scripting: https://lldb.llvm.org/use/python.html
- SB API Reference: https://lldb.llvm.org/resources/sbapi.html

### macOS Syscalls
- XNU Source (syscalls.master): https://github.com/apple/darwin-xnu
- macOS syscall numbers: https://opensource.apple.com/source/xnu/
- ARM64 calling convention: Apple ARM64 ABI

### strace Reference
- Linux strace man page: https://man7.org/linux/man-pages/man1/strace.1.html
- strace source: https://github.com/strace/strace

### Existing Tools
- Darling xtrace: https://github.com/darlinghq/darling/tree/master/src/xtrace
- dtruss (DTrace): Part of macOS, but requires SIP disabled

## Success Metrics

1. **Functionality**: Can trace 90% of typical macOS programs (excluding SIP-protected)
2. **Compatibility**: Output parseable by tools expecting strace format
3. **Performance**: < 5x overhead for typical workloads
4. **Quality**: > 80% test coverage, mypy type-check passes
5. **Usability**: Clear error messages, helpful documentation

## Questions / Clarifications Needed

None at this time - spec is complete and ready for implementation.

---

**Ready to implement**: This spec provides all necessary details for an LLM or developer to implement strace-darwin from scratch.
