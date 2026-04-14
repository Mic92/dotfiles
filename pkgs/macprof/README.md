# macprof

Wrap macOS `sample(1)`, `spindump(8)` and `xctrace(1)` and emit profiles that
[flamegraph.pl] and [speedscope] can actually read.

## Why

Apple's samplers are excellent but only produce indented text trees. Upstream
[FlameGraph] ships a collapser for `sample` only, nothing for `spindump`, and
neither produces multi-profile speedscope JSON — so a system-wide spindump
becomes one unreadable blob with no per-process selector. `macprof` runs the
sampler, parses the tree, drops idle/blocked threads, and writes both formats in
one go.

## Usage

```bash
# one process, 5 s (default) — writes both .folded and .speedscope.json to cwd
macprof sample <pid|name>
macprof sample Finder 10

# true on-CPU, whole system (Instruments Time Profiler, no sudo)
macprof xctrace
macprof xctrace 10 --attach Finder

# whole system wall-clock (sudo'd automatically)
macprof spindump
macprof spindump 10
macprof spindump --pid <pid> 5   # focus pid, still records all procs

# explicit output / format (options go after the subcommand)
macprof spindump 5 -o trace.speedscope.json   # ext picks format
macprof sample <pid> -o trace.folded
macprof spindump -f speedscope                # only speedscope, auto name

# keep wall-clock idle frames (off-CPU analysis) and the raw text report
macprof spindump 5 --keep-idle --keep-raw
```

`-o -` writes the `top` summary to stdout (handy for piping into a chat or
`less`).

Then view:

```bash
speedscope macprof-spindump-*.speedscope.json     # per-process dropdown, top-left
flamegraph.pl macprof-sample-*.folded > out.svg
```

## Output

| flag                   | result                                                          |
| ---------------------- | --------------------------------------------------------------- |
| _(none)_               | `macprof-<cmd>-<pid>.folded` **and** `….speedscope.json` in cwd |
| `-o x.json`            | speedscope only                                                 |
| `-o x.txt` / `-o x.md` | `top` text summary                                              |
| `-o x.*`               | folded only                                                     |
| `-f <fmt>`             | force format (overrides extension)                              |

### `top` format (LLM-friendly)

`-f top` (or `-o report.txt`) writes a bounded markdown-ish summary instead of
raw stacks: per process, total share, then hottest leaf functions each with one
elided root→leaf example path. Long C++/Swift symbols truncated. Size stays
under a few KB regardless of capture length — fits in a chat / commit message.

```
## Finder[1638]  —  2350 (100.0%)
   38.0%  getattrlistbulk
          … > _GetDirectoryURLs > DirEnumRead > NextEntryFromParent > getattrlistbulk
   19.8%  __mac_syscall
          … > canAccessURL:withAuditToken: > sandbox_check_by_audit_token > __mac_syscall
```

Speedscope JSON contains **one profile per process** (sorted by sample count),
so the top-left dropdown in speedscope works the way it does for Instruments or
Chrome traces.

## Which sampler

| cmd        | scope          | what it measures | root | notes                             |
| ---------- | -------------- | ---------------- | ---- | --------------------------------- |
| `xctrace`  | all / one proc | **on-CPU** only  | no   | accurate, needs Xcode CL tools    |
| `sample`   | one proc       | wall-clock       | no   | fast, always available            |
| `spindump` | all procs      | wall-clock       | yes  | shows blocking + cross-proc waits |

Use `xctrace` to find CPU hot spots, `spindump`/`sample` to find what threads
are _blocked on_.

## Idle filtering (sample/spindump only)

`sample` and `spindump` record wall-clock time, so parked threads
(`mach_msg2_trap`, `__workq_kernreturn`, `__psynch_cvwait`, `kevent`,
`psynch_cvcontinue`, …) dominate every graph. By default `macprof` drops a stack
when its deepest _named_ frame (skipping `???` kernel continuations) is one of
these, and drops `kernel_task[0]` entirely. Use `--keep-idle` for off-CPU /
blocking analysis.

The filter is heuristic, not a true on-CPU profiler — a thread with an
unsymbolicated event-loop leaf (e.g. `ChromeMain`, `uv_run`) will still show up.
For exact on-CPU data use `xctrace` Time Profiler instead.

## Notes

- `spindump` needs root; `macprof` prefixes `sudo` when not already root.
- spindump interval is fixed at 10 ms.
- Raw report is deleted after conversion unless `--keep-raw` is given.
- The folded format prefixes every stack with `procname[pid];…` so you can
  `grep '^Finder\[' trace.folded | flamegraph.pl` for a per-process SVG too.

[flamegraph.pl]: https://github.com/brendangregg/FlameGraph
[FlameGraph]: https://github.com/brendangregg/FlameGraph
[speedscope]: https://www.speedscope.app
