# minihydra

Per-commit nixpkgs eval/build differ. Answers: "what does this commit change?"

## Usage

```sh
# In a nixpkgs (or any flake) checkout. Worktree HEAD = "head" side,
# --base REV = "base" side. Default base is HEAD~1.
minihydra run --attr legacyPackages.x86_64-linux --base HEAD~1
minihydra run --attr hydraJobs.tested.x86_64-linux --build
minihydra run --flake .#hydraJobs --base origin/master

# Web UI
minihydra serve --db ~/.local/share/minihydra/minihydra.db
```

State lives in a single sqlite file (`$XDG_DATA_HOME/minihydra/minihydra.db` by
default). Copy it around freely.

## LLM-friendly interface

All read-only subcommands wrap the same JSON API the web UI uses, so the CLI and
`minihydra serve` agree on shapes.

```sh
minihydra list --json                      # all runs
minihydra show 7 --json                    # run metadata + summary
minihydra diff 7 --json                    # full diff (all sections)
minihydra diff 7 --section newly_broken    # one section
minihydra summary 7                        # compact text for LLM context
minihydra job 7 42                         # raw json-eval-jobs record
```

The HTTP API mirrors the CLI exactly — `GET /api/runs`, `/api/run/{id}`,
`/api/run/{id}/diff`, `/api/run/{id}/section/{name}`,
`/api/run/{id}/job/{job_id}`.

## Security

This tool exists to evaluate **arbitrary Nix code** at two revisions and
optionally build it. Treat it like `nix flake show` on an untrusted PR.

- Do not point `--base`/`--head` at refs you have not vetted. Nix evaluation
  triggers fetchers, IFD, and (on older nixes) `builtins.exec`. The git worktree
  checkout itself can run filters/hooks configured in the repo.
- The web UI has no authentication. It binds to `127.0.0.1` by default; binding
  to anything else requires `--allow-public` and is not recommended without a
  reverse proxy that adds auth.
- The sqlite DB is intended to be copied around. The build-log endpoint refuses
  to read paths outside `MINIHYDRA_LOG_ROOT` (set automatically by
  `minihydra serve`) so a tampered or stale `build_log_path` can't be used to
  exfiltrate arbitrary files.
- All subprocess calls use list arguments (no shell). The git worktree
  invocation uses a `--` sentinel so a hostile rev cannot be parsed as an
  option.

## Output

Per run, sqlite stores both sides' attribute results (name, drvPath, outputs,
error, system) and an optional build status row. The diff view groups attributes
into:

- `added` – present on head only
- `removed` – present on base only
- `changed` – drvPath differs
- `newly-broken` – evaluated on base, errored on head
- `newly-fixed` – errored on base, evaluated on head
- `still-broken` – errored on both, error message diff
- `unchanged` – identical drvPath
