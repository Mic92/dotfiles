---
name: macprof
description: Profile macOS processes (CPU hot spots, hangs, "what is X doing?"). Wraps sample/spindump/xctrace and prints a compact text summary or speedscope/flamegraph data.
---

Default invocation prints a bounded text summary to stdout — paste it back.

```bash
# What is process X doing? (one process, wall-clock, 5s)
macprof sample <pid|name> 5 -o -

# Where is CPU time going system-wide? (true on-CPU, needs Xcode CL tools)
macprof xctrace 5 -o -
macprof xctrace 5 --attach <pid|name> -o -

# What is everything blocked on? (wall-clock, all procs, needs sudo)
macprof spindump 5 -o - --keep-idle

# Files for the user to open interactively
macprof xctrace 10 -o cpu.speedscope.json   # → drop on https://speedscope.app
macprof spindump 10                          # → cwd: .folded + .speedscope.json
```

Pick sampler by question:

| question                          | command         |
| --------------------------------- | --------------- |
| process hung / spinning, why?     | `sample <name>` |
| what's burning CPU right now?     | `xctrace`       |
| system slow, who's blocking whom? | `spindump`      |

Flags: `-o -` stdout summary · `-o x.json` speedscope · `-o x.folded` flamegraph
· `--keep-idle` include blocked threads · `--keep-raw` keep raw report.

Pair with `heap <pid>` (object counts) and `lsof -p <pid>` (open files/dirs)
when memory, not CPU, is the symptom.
