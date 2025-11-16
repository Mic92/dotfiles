# Implement strace-darwin: System Call Tracer for macOS using LLDB

## Summary

Implement a strace-compatible system call tracer for macOS (Darwin) using LLDB Python API. This tool provides Linux strace-like functionality on macOS where dtruss/DTrace is blocked by System Integrity Protection (SIP).

**Full specification**: See `STRACE_DARWIN_SPEC.md` in this repository for complete implementation details.

## Quick Facts

- **Platform**: macOS ARM64 (Apple Silicon)
- **Language**: Python 3
- **Backend**: System LLDB Python API
- **License**: MIT
- **Location**: `pkgs/strace-darwin/`
- **Development**: Test-driven, 4 phases
- **Scope**: User processes only (SIP-protected binaries excluded)

## Why LLDB?

Unlike dtruss/DTrace which requires SIP disabled, LLDB can attach to user processes without system modifications:

✅ SIP-compatible - No security compromise needed
✅ Stable API - SB* classes are official public API
✅ Mach-based - Uses proper macOS debugging APIs (task_for_pid, thread_get_state)
✅ Available - Ships with Xcode Command Line Tools

## Architecture Overview

```
strace-darwin (Python CLI)
    ↓
LLDB Python API (SBDebugger, SBTarget, SBProcess)
    ↓
liblldb.so / LLDB.framework (SWIG bindings)
    ↓
debugserver (Mach APIs: task_for_pid, thread_get_state, vm_read)
    ↓
Target Process
```

**Syscall Interception Strategy**:
1. Set LLDB breakpoints on syscall wrapper functions in `libsystem_kernel` (`__open`, `__read`, etc.)
2. Extract syscall number from `x16` register (ARM64)
3. Parse arguments from `x0-x7` registers and memory
4. Format output to match strace conventions
5. Capture return value and display result

## Implementation Phases

### Phase 1: MVP - Basic Tracing (Weeks 1-2)

**Goal**: Prove concept with basic syscall tracing

**Deliverables**:
- [ ] LLDB Python wrapper for process attachment
- [ ] Breakpoint-based syscall interception
- [ ] Support 20 common syscalls with pretty-printing:
  - File: `open`, `openat`, `close`, `read`, `write`, `stat`, `fstat`, `lstat`
  - Process: `execve`, `fork`, `exit`, `getpid`, `getppid`
  - Memory: `mmap`, `munmap`, `brk`
  - Network: `socket`, `connect`, `bind`
  - Misc: `ioctl`, `fcntl`
- [ ] Basic argument parsing (int, string, pointer)
- [ ] Strace-like output format
- [ ] `-o <file>` output redirection
- [ ] Unit tests for argument parsing
- [ ] Integration tests (test with `ls`, `cat`, `echo`)
- [ ] Nix package definition
- [ ] Basic README with usage examples

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
- [ ] Syscall signature database
- [ ] Enhanced argument parsing:
  - Structures: `struct stat`, `struct sockaddr`, `struct timespec`
  - Arrays: `char *argv[]`, `struct iovec[]`
  - Flags: `O_RDONLY|O_CREAT`, `PROT_READ|PROT_WRITE`
  - Pointers with dereferencing
- [ ] `-yy` socket/fd decoding (show paths, addresses)
- [ ] `-t`, `-T` timing options
- [ ] `-c` summary statistics
- [ ] `-e trace=<set>` filtering
- [ ] `-p <pid>` attach to running process
- [ ] 50+ test cases
- [ ] Performance benchmarking

**Success Criteria**:
```bash
$ strace-darwin -c sleep 1
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 99.99    0.000050          50         1           nanosleep
  0.01    0.000000           0         5           mmap
------ ----------- ----------- --------- --------- ----------------
100.00    0.000050                     7           total

$ strace-darwin -yy nc localhost 8080
socket(AF_INET, SOCK_STREAM, IPPROTO_TCP) = 3
connect(3, {sa_family=AF_INET, sin_port=8080, sin_addr=127.0.0.1}, 16) = 0
```

### Phase 3: Process Management (Weeks 5-6)

**Goal**: Multi-process tracing

**Deliverables**:
- [ ] `-f` follow forks
- [ ] `-ff` separate output files per process
- [ ] Thread awareness
- [ ] Signal tracing
- [ ] Process tree tracking
- [ ] Multi-process output coordination
- [ ] Tests for fork/exec scenarios

**Success Criteria**:
```bash
$ strace-darwin -f sh -c "ls | grep tmp"
[pid 1234] execve("/bin/sh", ...) = 0
[pid 1234] pipe([3, 4]) = 0
[pid 1234] fork() = 1235
[pid 1235] execve("/bin/ls", ...) = 0
[pid 1234] fork() = 1236
[pid 1236] execve("/usr/bin/grep", ...) = 0
```

### Phase 4: Advanced Features (Week 7+)

**Goal**: Polish and advanced capabilities

**Deliverables**:
- [ ] `-e read=<set>` / `-e write=<set>` data dumping
- [ ] Advanced filtering (syscall classes)
- [ ] `-v` verbose mode
- [ ] `-x`, `-xx` hex output
- [ ] `-r` relative timestamps
- [ ] Performance optimization
- [ ] Complete documentation
- [ ] Man page
- [ ] Comparison guide: strace vs strace-darwin

## File Structure

```
pkgs/strace-darwin/
├── default.nix                    # Nix package definition
├── pyproject.toml                 # Python package metadata
├── README.md                      # User documentation
├── ARCHITECTURE.md                # Implementation details
├── LICENSE                        # MIT license
├── strace_darwin/
│   ├── __init__.py
│   ├── __main__.py                # CLI entry point
│   ├── tracer.py                  # Core LLDB tracer
│   ├── syscalls/
│   │   ├── __init__.py
│   │   ├── registry.py            # Syscall database
│   │   ├── arm64.py               # ARM64 syscall numbers
│   │   ├── parsers.py             # Argument parsers
│   │   └── formatters.py          # Output formatting
│   ├── breakpoints.py             # Breakpoint management
│   ├── process.py                 # Process lifecycle
│   └── statistics.py              # Statistics for -c
├── tests/
│   ├── test_syscall_parsing.py   # Unit tests
│   ├── test_formatters.py         # Output tests
│   ├── test_integration.py        # Integration tests
│   └── fixtures/
│       ├── simple_program.c
│       └── fork_exec.c
└── examples/
    └── usage_examples.md
```

## Core Implementation Skeleton

### 1. LLDB Tracer (`strace_darwin/tracer.py`)

```python
import lldb
import sys
import os

class LLDBTracer:
    def __init__(self, args):
        self.debugger = lldb.SBDebugger.Create()
        self.target = None
        self.process = None
        self.args = args

    def attach_to_process(self, pid: int) -> bool:
        """Attach to existing process"""
        error = lldb.SBError()
        self.target = self.debugger.CreateTarget("")
        attach_info = lldb.SBAttachInfo(pid)
        self.process = self.target.Attach(attach_info, error)

        if not error.Success():
            print(f"Failed to attach: {error.GetCString()}", file=sys.stderr)
            return False

        self._setup_breakpoints()
        return True

    def launch_process(self, executable: str, args: list) -> bool:
        """Launch and trace new process"""
        error = lldb.SBError()
        self.target = self.debugger.CreateTarget(executable, None, None, True, error)

        if not error.Success():
            print(f"Failed to create target: {error.GetCString()}", file=sys.stderr)
            return False

        self._setup_breakpoints()

        launch_info = lldb.SBLaunchInfo(args)
        launch_info.SetWorkingDirectory(os.getcwd())
        self.process = self.target.Launch(launch_info, error)

        if not error.Success():
            print(f"Failed to launch: {error.GetCString()}", file=sys.stderr)
            return False

        return True

    def _setup_breakpoints(self):
        """Set breakpoints on syscall wrappers"""
        # TODO: Set breakpoints on __open, __read, __write, etc.
        # Use: self.target.BreakpointCreateByName("__open")
        pass

    def syscall_callback(self, frame, bp_loc):
        """Called when syscall breakpoint hits"""
        # Extract syscall number from x16
        x16 = frame.FindRegister("x16")
        syscall_num = x16.GetValueAsUnsigned()

        # Extract arguments from x0-x7
        args = []
        for i in range(8):
            reg = frame.FindRegister(f"x{i}")
            args.append(reg.GetValueAsUnsigned())

        # TODO: Parse and format syscall
        # TODO: Print output

        return True  # Continue execution

    def run(self) -> int:
        """Main event loop"""
        listener = self.debugger.GetListener()
        event = lldb.SBEvent()

        while True:
            if listener.WaitForEvent(1, event):
                if lldb.SBProcess.EventIsProcessEvent(event):
                    state = lldb.SBProcess.GetStateFromEvent(event)

                    if state == lldb.eStateExited:
                        return self.process.GetExitStatus()
                    elif state == lldb.eStateCrashed:
                        print("Process crashed", file=sys.stderr)
                        return 1

        return 0
```

### 2. Syscall Registry (`strace_darwin/syscalls/registry.py`)

```python
from dataclasses import dataclass
from typing import List, Optional
from enum import Enum

class ArgType(Enum):
    INT = "int"
    STRING = "char*"
    PTR = "void*"
    FLAGS = "flags"
    # Add more types...

@dataclass
class SyscallArg:
    name: str
    type: ArgType

@dataclass
class SyscallInfo:
    number: int
    name: str
    args: List[SyscallArg]
    return_type: str

class SyscallRegistry:
    def __init__(self):
        self.syscalls = {}
        self._register_syscalls()

    def _register_syscalls(self):
        """Register all ARM64 syscalls"""
        self.register(SyscallInfo(
            number=5,
            name="open",
            args=[
                SyscallArg("path", ArgType.STRING),
                SyscallArg("flags", ArgType.FLAGS),
                SyscallArg("mode", ArgType.FLAGS),
            ],
            return_type="int"
        ))
        # TODO: Add all syscalls

    def register(self, syscall: SyscallInfo):
        self.syscalls[syscall.number] = syscall

    def get_syscall(self, number: int) -> Optional[SyscallInfo]:
        return self.syscalls.get(number)

    def format_args(self, syscall: SyscallInfo, raw_args: List[int], frame) -> str:
        """Format arguments for display"""
        formatted = []

        for i, arg_spec in enumerate(syscall.args):
            raw_val = raw_args[i]

            if arg_spec.type == ArgType.STRING:
                # Read string from memory
                string_val = self._read_string(frame.GetProcess(), raw_val)
                formatted.append(f'"{string_val}"')
            elif arg_spec.type == ArgType.INT:
                formatted.append(str(raw_val))
            elif arg_spec.type == ArgType.PTR:
                formatted.append(f"0x{raw_val:x}" if raw_val else "NULL")
            # TODO: Handle more types

        return ", ".join(formatted)

    def _read_string(self, process, addr: int, max_len: int = 256) -> str:
        """Read null-terminated string from process memory"""
        error = lldb.SBError()
        data = process.ReadMemory(addr, max_len, error)

        if not error.Success():
            return f"<unreadable>"

        try:
            null_pos = data.index(b'\0')
            return data[:null_pos].decode('utf-8', errors='replace')
        except ValueError:
            return data.decode('utf-8', errors='replace') + '...'
```

### 3. CLI Entry Point (`strace_darwin/__main__.py`)

```python
import argparse
import sys
from .tracer import LLDBTracer

def main():
    parser = argparse.ArgumentParser(description='System call tracer for macOS')

    parser.add_argument('-o', '--output', help='Write output to file')
    parser.add_argument('-c', '--summary-only', action='store_true')
    parser.add_argument('-e', '--expr', help='Filter expression')
    parser.add_argument('-p', '--attach', type=int, metavar='PID')
    parser.add_argument('-t', '--absolute-timestamps', action='store_true')
    parser.add_argument('-T', '--syscall-times', action='store_true')
    parser.add_argument('-yy', '--decode-fds-extended', action='store_true')
    parser.add_argument('-f', '--follow-forks', action='store_true')
    parser.add_argument('command', nargs='*')

    args = parser.parse_args()

    if not args.attach and not args.command:
        parser.error('Must specify either -p PID or command')

    if args.output:
        sys.stderr = open(args.output, 'w')

    tracer = LLDBTracer(args)
    return tracer.run()

if __name__ == '__main__':
    sys.exit(main())
```

## Nix Integration

### Package Definition (`pkgs/strace-darwin/default.nix`)

```nix
{ lib, python3Packages, lldb, stdenv }:

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

  checkPhase = ''
    pytest tests/
  '';

  meta = with lib; {
    description = "System call tracer for macOS using LLDB";
    homepage = "https://github.com/Mic92/dotfiles";
    license = licenses.mit;
    platforms = platforms.darwin;
  };
}
```

### Registration in `pkgs/flake-module.nix`

```nix
# Add to Darwin-only packages (around line 31-34)
// pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
  blueutil = pkgs.callPackage ./blueutil { };
  systemctl-macos = pkgs.callPackage ./systemctl { };
  strace-darwin = pkgs.callPackage ./strace-darwin { };  # ADD THIS
}
```

### System Integration

Add to `machines/evo/configuration.nix`:

```nix
environment.systemPackages = [
  self.packages.${pkgs.stdenv.hostPlatform.system}.strace-darwin
];

# Optional: Create strace alias to match Linux usage
environment.shellAliases = {
  strace = "strace-darwin -yy";
};
```

## Testing Strategy

### Unit Tests (`tests/test_syscall_parsing.py`)

```python
import pytest
from strace_darwin.syscalls.registry import SyscallRegistry

def test_syscall_registration():
    registry = SyscallRegistry()
    open_syscall = registry.get_syscall(5)
    assert open_syscall is not None
    assert open_syscall.name == "open"
    assert len(open_syscall.args) == 3

def test_flag_formatting():
    # Test O_RDONLY | O_CREAT formatting
    pass
```

### Integration Tests (`tests/test_integration.py`)

```python
import subprocess
import pytest

def test_trace_echo():
    result = subprocess.run(
        ['strace-darwin', '/bin/echo', 'hello'],
        capture_output=True,
        text=True
    )
    assert result.returncode == 0
    assert 'write(' in result.stderr
    assert 'exit(' in result.stderr

def test_output_file(tmp_path):
    output_file = tmp_path / "trace.out"
    subprocess.run(['strace-darwin', '-o', str(output_file), '/bin/ls'])
    assert output_file.exists()
    assert 'open(' in output_file.read_text()

def test_attach_to_process():
    # Start sleep process, attach, verify tracing
    pass
```

## Known Limitations

Document in README.md:

1. **SIP-Protected Binaries**: Cannot trace `/System/*`, `/usr/bin/*` without disabling SIP (out of scope)
2. **Hardened Runtime**: Apps without `get-task-allow` entitlement cannot be traced
3. **Performance**: LLDB-based tracing has higher overhead than kernel tracing
4. **Platform**: ARM64 only (Intel Macs not supported in MVP)
5. **Syscall Pretty-Printing**: All syscalls traced, but only important ones have detailed formatting

## Acceptance Criteria

### Phase 1 (MVP)
- [ ] Traces simple programs: `ls`, `cat`, `echo`
- [ ] Output matches strace format
- [ ] `-o` option works
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Nix package builds
- [ ] Integrated into dotfiles flake

### Phase 2 (Enhanced)
- [ ] All 200+ syscalls intercepted
- [ ] 50+ syscalls have pretty-printing
- [ ] `-c`, `-e`, `-p`, `-t`, `-T`, `-yy` work
- [ ] Can attach to running processes
- [ ] Performance < 5x overhead
- [ ] 50+ test cases pass

### Phase 3 (Process Management)
- [ ] Traces forking programs: `sh -c "ls | grep tmp"`
- [ ] `-f` follows children
- [ ] Output distinguishes PIDs
- [ ] Handles complex fork/exec

### Phase 4 (Advanced)
- [ ] All planned options implemented
- [ ] Performance optimized
- [ ] Complete documentation
- [ ] Comparison guide available

## Resources

### Documentation
- **Full Spec**: `STRACE_DARWIN_SPEC.md` in this repo
- **LLDB Python API**: https://lldb.llvm.org/python_api.html
- **LLDB Scripting**: https://lldb.llvm.org/use/python.html
- **strace man page**: https://man7.org/linux/man-pages/man1/strace.1.html

### macOS Syscalls
- **XNU Source**: https://github.com/apple/darwin-xnu
- **ARM64 ABI**: Apple ARM64 calling convention docs

### Reference Implementations
- **Darling xtrace**: https://github.com/darlinghq/darling/tree/master/src/xtrace
- **Linux strace**: https://github.com/strace/strace

## Getting Started

1. Read `STRACE_DARWIN_SPEC.md` for complete details
2. Create `pkgs/strace-darwin/` directory
3. Implement Phase 1 MVP with tests
4. Integrate into Nix flake
5. Test on real programs
6. Iterate through remaining phases

## Questions?

Refer to `STRACE_DARWIN_SPEC.md` for:
- Detailed architecture explanations
- Complete code examples
- Technical decision rationale
- ARM64 syscall mechanics
- LLDB internals

---

**Labels**: enhancement, macOS, Darwin, debugging, tooling
**Estimated Effort**: 6-8 weeks for all phases
**Priority**: Medium
**Assignee**: Open for implementation
