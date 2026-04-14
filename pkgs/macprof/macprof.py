#!/usr/bin/env python3
"""macprof — wrap macOS `sample`/`spindump` and emit folded or speedscope output.

WHY: Apple ships great samplers but no machine-readable output. FlameGraph's
upstream only handles `sample` (awk), and nothing produces multi-profile
speedscope JSON (one profile per process) which is what makes spindump
actually browsable. This puts both behind one CLI so the workflow is
`macprof spindump -o sys.speedscope.json && speedscope sys.speedscope.json`.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from collections import defaultdict
from contextlib import contextmanager
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING
from xml.etree import ElementTree as ET

if TYPE_CHECKING:
    from collections.abc import Callable, Iterable, Iterator
    from typing import IO

type Folded = tuple[str, int]  # ("proc;frame;frame", samples)

# Function names that mean "thread parked / waiting", i.e. off-CPU. Spindump
# and sample both record wall-clock, so without filtering these dominate every
# graph and bury the on-CPU work. Matched by exact function token (the part
# before ` (lib)`), not prefix, so `read` doesn't swallow `readdir` etc.
IDLE_FUNCS = frozenset(
    {
        "mach_msg2_trap",
        "mach_msg_trap",
        "mach_msg_overwrite",
        "mach_msg",
        "__workq_kernreturn",
        "__psynch_cvwait",
        "__psynch_mutexwait",
        "kevent",
        "kevent_id",
        "kevent_qos",
        "__select",
        "__semwait_signal",
        "__sigsuspend",
        "__sigsuspend_nocancel",
        "__ulock_wait",
        "__ulock_wait2",
        "semaphore_wait_trap",
        "semaphore_timedwait_trap",
        "swtch_pri",
        "read",
        "__read",
        "__read_nocancel",
        "poll",
        "__wait4",
        "__accept",
        "__recvmsg",
        "__sigwait",
        "guarded_pwrite_np",
        "kevent64",
        # libdispatch / pthread park points (frame *above* the syscall when
        # the syscall itself isn't symbolicated):
        "_dispatch_sig_thread",
        "_dispatch_semaphore_wait_slow",
        "_dispatch_wait_on_address",
        "_dispatch_event_loop_wait_for_ownership",
        "_pthread_cond_wait",
        "start_wqthread",
        # kernel-side sleep continuations seen under spindump:
        "psynch_cvcontinue",
        "lck_mtx_sleep",
        "thread_block_reason",
        "semaphore_wait_continue",
    }
)

# `Process:  Name [pid]` header — identical in sample and spindump.
_PROC = re.compile(r"^Process:\s+(.+?)\s+\[(\d+)\]")


def _proc_name(m: re.Match[str]) -> str:
    return f"{m.group(1)}[{m.group(2)}]"


def _frame(func: str, lib: str | None) -> str:
    return f"{func} ({lib})" if lib else func


# ── spindump ───────────────────────────────────────────────────────────────
# Tree lines: `<indent>*?<count>  <func> + N (<lib> + N) [0x..]` (lib optional).
_SD_LINE = re.compile(
    r"^(\s*)\*?(\d+)\s+(.+?)"
    r"(?:\s+\+ \d+)?"
    r"(?:\s+\((.+?)(?:\s+\+\s+\d+)?\))?"
    r"\s+\[0x[0-9a-f]+\].*$"
)
_SD_THREAD = re.compile(r"^\s*Thread\s+0x[0-9a-f]+", re.IGNORECASE)


@dataclass(slots=True)
class _TreeFolder:
    """Shared inclusive-count call-tree walker for sample/spindump.

    Both tools print trees where a node's count includes all descendants, so
    self time = count - sum(direct children). Push on the way down, emit on
    pop. Kept as a class so the two parsers don't duplicate the bookkeeping.
    """

    proc: str = "unknown"
    _stack: list[tuple[int, int, str]] = field(default_factory=list)
    _child_sum: list[int] = field(default_factory=list)

    def flush_to(self, depth: int) -> Iterator[Folded]:
        stack, child_sum = self._stack, self._child_sum
        while stack and stack[-1][0] >= depth:
            _d, cnt, frame = stack.pop()
            csum = child_sum.pop()
            self_time = cnt - csum
            if self_time > 0:
                prefix = ";".join(f for _, _, f in stack)
                full = f"{prefix};{frame}" if prefix else frame
                yield f"{self.proc};{full}", self_time
            if child_sum:
                child_sum[-1] += cnt

    def push(self, depth: int, count: int, frame: str) -> Iterator[Folded]:
        yield from self.flush_to(depth)
        self._stack.append((depth, count, frame))
        self._child_sum.append(0)


def fold_spindump(lines: Iterable[str]) -> Iterator[Folded]:
    t = _TreeFolder()
    for raw in lines:
        if m := _PROC.match(raw):
            yield from t.flush_to(-1)
            t.proc = _proc_name(m)
            continue
        if _SD_THREAD.match(raw):
            yield from t.flush_to(-1)
            continue
        m = _SD_LINE.match(raw)
        if not m:
            continue
        indent, cnt, func, lib = m.groups()
        yield from t.push(len(indent) // 2, int(cnt), _frame(func, lib))
    yield from t.flush_to(-1)


# ── sample ─────────────────────────────────────────────────────────────────
# Tree lines: `<indent><count> <func>  (in <lib>) ...`. Indent uses a mix of
# spaces and box-drawing chars (`+ ! : |`), one char per level after the
# leading 4-space gutter. Anything after `(in lib)` (offsets, addresses,
# `load address …`) is noise for our purposes.
_SAMPLE_LINE = re.compile(
    r"^(?P<ind>[\s+!|:]*?)(?P<cnt>\d+)\s+(?P<func>.+?)"
    r"(?:\s+\(in (?P<lib>[^)]+)\).*)?"
    r"(?:\s+(?:\+ \d+\s+)?\[[^]]+\])?\s*$"
)


def fold_sample(lines: Iterable[str]) -> Iterator[Folded]:
    t = _TreeFolder()
    in_tree = False
    for raw in lines:
        if m := _PROC.match(raw):
            t.proc = _proc_name(m)
            continue
        if raw.startswith("Call graph:"):
            in_tree = True
            continue
        if in_tree and raw.strip() == "":
            # Tree ends at first blank line; "Total number in stack" / binary
            # images sections follow and would otherwise mis-parse.
            yield from t.flush_to(-1)
            in_tree = False
            continue
        if not in_tree:
            continue
        m = _SAMPLE_LINE.match(raw)
        if not m:
            continue
        # sample(1) gutter is 4 spaces, then one indent char per level.
        depth = max(len(m.group("ind")) - 4, 0)
        yield from t.push(depth, int(m.group("cnt")), _frame(m["func"], m["lib"]))
    yield from t.flush_to(-1)


# ── xctrace ────────────────────────────────────────────────────────────────
# `xctrace export --xpath …/table[@schema="time-profile"]` emits one <row> per
# sample with <process>, <weight>, <backtrace><frame name=…><binary name=…/>.
# Every element is interned: first occurrence carries id= + payload, later
# occurrences carry only ref= back to that id. We resolve refs in one pass by
# memoising every element keyed on its id as we walk.
_TIME_PROFILE_XPATH = '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]'


class _RefTable:
    """id/ref interning resolver for xctrace XML."""

    def __init__(self) -> None:
        self._by_id: dict[str, ET.Element] = {}

    def see(self, e: ET.Element) -> None:
        if (i := e.get("id")) is not None:
            self._by_id[i] = e

    def deref(self, e: ET.Element | None) -> ET.Element | None:
        if e is None:
            return None
        self.see(e)
        if (r := e.get("ref")) is not None:
            return self._by_id.get(r)
        return e


def fold_xctrace(xml_path: Path) -> Iterator[Folded]:
    refs = _RefTable()
    deref = refs.deref

    def frame_name(f: ET.Element) -> str:
        # Frames are interned too: later occurrences are bare `<frame ref="N"/>`
        # with no name/binary. Dereference *before* reading attributes or 80%+
        # of frames come out as `???`.
        fr = deref(f)
        f = fr if fr is not None else f
        b = deref(f.find("binary"))
        return _frame(f.get("name", "???"), b.get("name") if b is not None else None)

    # iterparse keeps memory bounded; the export for a few seconds is already
    # multiple MB and grows linearly with duration * cores.
    for _, elem in ET.iterparse(xml_path, events=("end",)):  # noqa: S314
        refs.see(elem)
        if elem.tag != "row":
            continue
        proc = deref(elem.find("process"))
        bt = deref(elem.find("backtrace"))
        w = deref(elem.find("weight"))
        if proc is None or bt is None:
            continue
        # weight is ns; treat 1 ms as 1 sample so numbers are comparable to
        # sample(1)/spindump 1 ms / 10 ms ticks.
        weight = max(int(w.text or "1000000") // 1_000_000, 1) if w is not None else 1
        # frames are leaf-first; flamegraph wants root-first.
        frames = [frame_name(fr) for fr in reversed(list(bt))]
        if not frames:
            elem.clear()
            continue
        proc_fmt = proc.get("fmt", "unknown")  # "name (pid)" → "name[pid]"
        proc_name = re.sub(r" \((\d+)\)$", r"[\1]", proc_fmt)
        yield f"{proc_name};{';'.join(frames)}", weight
        elem.clear()


def aggregate(folded: Iterable[Folded]) -> list[Folded]:
    """Sum weights of identical stacks.

    xctrace emits one row per 1 ms tick so the same stack repeats hundreds of
    times; collapsing here keeps the .folded/.json output an order of
    magnitude smaller without changing the flamegraph.
    """
    acc: dict[str, int] = defaultdict(int)
    for s, n in folded:
        acc[s] += n
    return list(acc.items())


# ── output ─────────────────────────────────────────────────────────────────
def _func_of(frame: str) -> str:
    """Return the bare function name of a frame, i.e. strip ` (lib)`."""
    return frame.split(" (", 1)[0]


def _named_leaf(parts: list[str]) -> str:
    """Deepest frame whose function isn't `???` (skip kernel continuations)."""
    return next((f for p in reversed(parts) if (f := _func_of(p)) != "???"), "???")


def filter_idle(folded: Iterable[Folded]) -> Iterator[Folded]:
    """Drop stacks whose deepest *meaningful* frame is a blocking syscall.

    Spindump appends a kernel continuation (`*N ??? (kernel.release.…)`)
    below the userspace syscall, so the literal leaf is `???` and a naive
    leaf check misses ~everything. We walk up past `???` frames first.
    Also drop `kernel_task[0]` outright — it's scheduler/idle threads with
    no symbols and pushes every real process off the top of the dropdown.
    """
    for stack, n in folded:
        parts = stack.split(";")
        if parts[0].startswith("kernel_task["):
            continue
        leaf_func = _named_leaf(parts)
        if leaf_func in IDLE_FUNCS:
            continue
        # Entire user stack unsymbolicated, ending in a kernel `???` — no way
        # to tell what it's doing but in practice these are parked threads of
        # suspended app extensions; keep them out of the on-CPU view.
        if leaf_func == parts[0] and "kernel" in parts[-1]:
            continue
        yield stack, n


@contextmanager
def _open_out(out: Path) -> Iterator[IO[str]]:
    """`-` writes to stdout so `macprof … -o -` pipes straight into a chat/log."""
    if str(out) == "-":
        yield sys.stdout
        return
    with out.open("w", encoding="utf-8") as fh:
        yield fh


def write_folded(folded: Iterable[Folded], out: Path) -> None:
    with _open_out(out) as fh:
        for stack, n in folded:
            fh.write(f"{stack} {n}\n")


def write_top(
    folded: Iterable[Folded], out: Path, *, max_procs: int = 15, max_leaves: int = 8
) -> None:
    """Emit a compact, bounded-size text summary for humans and LLMs.

    Folded lines are hundreds of chars wide and speedscope JSON is
    index-encoded — neither pastes well into a chat. This format gives, per
    process: total share, then the hottest *leaf* functions with one example
    call path (root→leaf, middle elided). Size is O(max_procs * max_leaves)
    regardless of capture length.
    """
    folded = list(folded)
    grand = sum(n for _, n in folded) or 1
    by_proc: dict[str, list[Folded]] = defaultdict(list)
    for s, n in folded:
        by_proc[s.split(";", 1)[0]].append((s, n))

    def short(f: str, n: int = 80) -> str:
        # C++/Swift demangled symbols routinely exceed 200 chars; an LLM only
        # needs the head to recognise the function.
        return f if len(f) <= n else f[: n - 1] + "…"

    def short_path(stack: str, keep: int = 4) -> str:
        frames = [short(_func_of(f)) for f in stack.split(";")[1:]]
        if len(frames) <= 2 * keep:
            return " > ".join(frames)
        return " > ".join(
            [*frames[:keep], f"…({len(frames) - 2 * keep})…", *frames[-keep:]]
        )

    proc_total = {p: sum(n for _, n in v) for p, v in by_proc.items()}
    procs = sorted(proc_total, key=lambda p: -proc_total[p])
    with _open_out(out) as fh:
        fh.write(f"# macprof top  —  {grand} samples, {len(procs)} processes\n\n")
        for proc in procs[:max_procs]:
            ptotal = proc_total[proc]
            fh.write(f"## {proc}  —  {ptotal} ({100 * ptotal / grand:.1f}%)\n")
            # Rank by leaf function; per leaf keep (sum, heaviest example stack).
            leaves: dict[str, tuple[int, int, str]] = {}
            for s, n in by_proc[proc]:
                leaf = _named_leaf(s.split(";"))
                tot, best_n, best_s = leaves.get(leaf, (0, 0, s))
                if n > best_n:
                    best_n, best_s = n, s
                leaves[leaf] = (tot + n, best_n, best_s)
            for leaf, (n, _, ex) in sorted(leaves.items(), key=lambda kv: -kv[1][0])[
                :max_leaves
            ]:
                fh.write(f"  {100 * n / ptotal:5.1f}%  {short(leaf, 100)}\n")
                fh.write(f"         {short_path(ex)}\n")
            fh.write("\n")
        if len(procs) > max_procs:
            rest = sum(proc_total[p] for p in procs[max_procs:])
            fh.write(
                f"… {len(procs) - max_procs} more processes "
                f"({100 * rest / grand:.1f}%)\n"
            )


def write_speedscope(folded: Iterable[Folded], out: Path, name: str) -> None:
    """Emit speedscope file-format JSON with one sampled profile per process.

    The first `;`-component is treated as the process key so the speedscope
    UI gets its profile dropdown back (folded files alone show as one blob).
    """
    frames: list[dict[str, str]] = []
    frame_idx: dict[str, int] = {}

    def idx(f: str) -> int:
        i = frame_idx.get(f)
        if i is None:
            i = len(frames)
            frame_idx[f] = i
            frames.append({"name": f})
        return i

    per_proc: dict[str, list[tuple[list[int], int]]] = defaultdict(list)
    proc_total: dict[str, int] = defaultdict(int)
    for stack, n in folded:
        parts = stack.split(";")
        proc = parts[0]
        rest = parts[1:] or [proc]
        per_proc[proc].append(([idx(p) for p in rest], n))
        proc_total[proc] += n

    profiles = []
    for proc in sorted(per_proc, key=lambda p: -proc_total[p]):
        samples, weights = [], []
        for s, w in per_proc[proc]:
            samples.append(s)
            weights.append(w)
        profiles.append(
            {
                "type": "sampled",
                "name": proc,
                "unit": "none",
                "startValue": 0,
                "endValue": proc_total[proc],
                "samples": samples,
                "weights": weights,
            }
        )

    doc = {
        "$schema": "https://www.speedscope.app/file-format-schema.json",
        "exporter": "macprof",
        "name": name,
        "activeProfileIndex": 0,
        "shared": {"frames": frames},
        "profiles": profiles,
    }
    with _open_out(out) as fh:
        json.dump(doc, fh)


# ── capture ────────────────────────────────────────────────────────────────
@contextmanager
def capture(kind: str, argv: list[str], *, keep: bool) -> Iterator[Path]:
    """Run a sampler that writes to a temp file and yield its path.

    Context manager so the raw report (and the tempfile fd) are cleaned up on
    every exit path, including parse errors — the previous mkstemp approach
    leaked both the fd and the file on exceptions.
    """
    with tempfile.NamedTemporaryFile(
        prefix=f"macprof-{kind}-", suffix=".txt", delete=False
    ) as tmp:
        path = Path(tmp.name)
    try:
        rc = subprocess.run([*argv, "-file", str(path)], check=False).returncode
        if rc != 0:
            sys.stderr.write(f"macprof: {argv[0]} exited {rc}\n")
            raise SystemExit(rc)
        yield path
    finally:
        if keep:
            sys.stderr.write(f"macprof: raw report kept at {path}\n")
        else:
            path.unlink(missing_ok=True)


def sample_argv(target: str, seconds: int) -> list[str]:
    return ["/usr/bin/sample", target, str(seconds)]


@contextmanager
def capture_xctrace(seconds: int, target: str | None, *, keep: bool) -> Iterator[Path]:
    """Record Time Profiler and export the time-profile table as XML.

    Two-stage capture (record .trace → export XML) wrapped so the bulky
    .trace bundle is removed even when export fails. The exported XML is what
    `fold_xctrace` consumes.
    """
    base = Path(tempfile.mkdtemp(prefix="macprof-xctrace-"))
    trace, xml = base / "rec.trace", base / "rec.xml"
    record = [
        "xctrace",
        "record",
        "--template",
        "Time Profiler",
        "--time-limit",
        f"{seconds}s",
        "--output",
        str(trace),
    ]
    record += ["--attach", target] if target else ["--all-processes"]
    export = [
        "xctrace",
        "export",
        "--input",
        str(trace),
        "--xpath",
        _TIME_PROFILE_XPATH,
        "--output",
        str(xml),
    ]
    try:
        for argv in (record, export):
            rc = subprocess.run(argv, check=False).returncode
            if rc != 0:
                sys.stderr.write(f"macprof: {' '.join(argv[:2])} exited {rc}\n")
                raise SystemExit(rc)
        yield xml
    finally:
        if keep:
            sys.stderr.write(f"macprof: raw trace kept at {base}\n")
        else:
            shutil.rmtree(base, ignore_errors=True)


def spindump_argv(seconds: int, pid: int | None) -> list[str]:
    argv = ["/usr/sbin/spindump"]
    argv += [str(pid)] if pid is not None else ["-notarget"]
    argv += [str(seconds), "10"]
    return argv if os.geteuid() == 0 else ["sudo", *argv]


# ── cli ────────────────────────────────────────────────────────────────────
def plan_outputs(
    output: Path | None, fmt: str | None, cmd: str
) -> list[tuple[str, Path]]:
    """Decide what to write.

    Default (no -o/-f): emit BOTH `<base>.folded` and `<base>.speedscope.json`
    into cwd so the result is immediately usable with either flamegraph.pl or
    speedscope without re-running the capture.
    """
    suffix = {"folded": ".folded", "speedscope": ".speedscope.json", "top": ".txt"}
    by_ext = {".json": "speedscope", ".txt": "top", ".md": "top"}
    if output is not None:
        chosen = fmt or (
            "top" if str(output) == "-" else by_ext.get(output.suffix, "folded")
        )
        return [(chosen, output)]
    base = f"macprof-{cmd}-{os.getpid()}"
    fmts = [fmt] if fmt else ["folded", "speedscope"]
    return [(f, Path(base + suffix[f])) for f in fmts]


def _common_parser() -> argparse.ArgumentParser:
    # Shared options live on a parent parser attached to each subcommand so
    # they appear *after* the subcommand (`macprof sample … -o x`). They are
    # deliberately not also on the top-level parser: argparse would let the
    # subparser's `default=None` clobber a value given before the subcommand.
    p = argparse.ArgumentParser(add_help=False)
    p.add_argument(
        "-o",
        "--output",
        type=Path,
        help="output file (.json=speedscope, .txt/.md=top, else folded; "
        "`-`=stdout as top). Omit to write folded+speedscope to cwd.",
    )
    p.add_argument("-f", "--format", choices=("folded", "speedscope", "top"))
    p.add_argument(
        "--keep-idle",
        action="store_true",
        help="keep blocked/waiting leaf frames and kernel_task (default "
        "drops them so on-CPU work is visible; no-op for xctrace)",
    )
    p.add_argument(
        "--keep-raw",
        action="store_true",
        help="do not delete the captured sample/spindump text",
    )
    return p


def main() -> None:
    ap = argparse.ArgumentParser(
        prog="macprof",
        description="Capture macOS sample(1)/spindump(8)/xctrace(1) and emit "
        "folded stacks, speedscope JSON, or a compact 'top' text summary.",
        epilog=(
            "recipes:\n"
            "  macprof sample Finder 5 -o -          # text summary to stdout\n"
            "  macprof xctrace 5 -o cpu.json         # speedscope, on-CPU\n"
            "  macprof spindump 5                    # folded + speedscope to cwd\n"
            "  macprof spindump 5 -o - --keep-idle   # off-CPU summary\n"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    common = _common_parser()
    sub = ap.add_subparsers(dest="cmd", required=True)

    sp = sub.add_parser(
        "sample", parents=[common], help="profile one process via sample(1)"
    )
    sp.add_argument("target", help="pid or process name (passed to sample(1))")
    sp.add_argument("seconds", type=int, nargs="?", default=5)

    sd = sub.add_parser(
        "spindump", parents=[common], help="profile whole system via spindump(8)"
    )
    sd.add_argument("seconds", type=int, nargs="?", default=5)
    sd.add_argument("--pid", type=int, help="target pid (still records all procs)")

    xc = sub.add_parser(
        "xctrace",
        parents=[common],
        help="on-CPU profile via xctrace Time Profiler (true on-CPU, no idle filter needed)",
    )
    xc.add_argument("seconds", type=int, nargs="?", default=5)
    xc.add_argument(
        "--attach", metavar="PID|NAME", help="target one process (default: all)"
    )

    args = ap.parse_args()

    def _read(path: Path) -> Iterator[str]:
        with path.open(encoding="utf-8", errors="replace") as fh:
            yield from fh

    keep = args.keep_raw
    fold: Callable[[Path], Iterator[Folded]]
    match args.cmd:
        case "sample":
            cap = capture("sample", sample_argv(args.target, args.seconds), keep=keep)
            fold = lambda p: fold_sample(_read(p))  # noqa: E731
        case "spindump":
            cap = capture("spindump", spindump_argv(args.seconds, args.pid), keep=keep)
            fold = lambda p: fold_spindump(_read(p))  # noqa: E731
        case "xctrace":
            cap = capture_xctrace(args.seconds, args.attach, keep=keep)
            fold = fold_xctrace
        case _:
            ap.error("unreachable")

    with cap as raw:
        folded = aggregate(fold(raw))
        # xctrace Time Profiler is already on-CPU; only the wall-clock
        # samplers need the heuristic.
        if args.cmd != "xctrace" and not args.keep_idle:
            folded = list(filter_idle(folded))
        if not folded:
            sys.stderr.write(
                "macprof: warning: no on-CPU samples survived; "
                "re-run with --keep-idle for off-CPU view\n"
            )

        emit(folded, plan_outputs(args.output, args.format, args.cmd), raw.name)


def emit(folded: list[Folded], outputs: list[tuple[str, Path]], src_name: str) -> None:
    for fmt, out in outputs:
        match fmt:
            case "speedscope":
                write_speedscope(folded, out, name=src_name)
            case "top":
                write_top(folded, out)
            case _:
                write_folded(folded, out)
        if str(out) != "-":
            sys.stderr.write(f"macprof: {fmt}: {out}\n")


if __name__ == "__main__":
    main()
