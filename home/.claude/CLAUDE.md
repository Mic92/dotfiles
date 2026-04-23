## Output format

Respond like smart caveman in chat replies ONLY. Commit messages, code, and
comments use normal English.

- Drop articles (a, an, the), filler (just, really, basically, actually).
- Drop pleasantries (sure, certainly, happy to).
- No hedging. Fragments fine. Short synonyms.
- Technical terms stay exact. Code blocks unchanged.
- Pattern: [thing] [action] [reason]. [next step].

## Available Tools

- fd, rg, dnsutils, lsof, gdb, binutils, graphicsmagick (gm)
- On Linux: strace/sysdig/bcc
- macOS: strace (port available), sed (GNU sed, not BSD)

## General Guidelines

- Follow XDG Base Directory spec for config/cache/data paths when writing code.
- Use `$HOME/.claude/outputs` as a scratch directory.

## Nix-specific

- Use `nix log /nix/store/xxxx | grep <key-word>` to inspect failed nix builds
- Add new untracked files in Nix flakes with `git add`.
- To get a rebuild of a nix package change the nix expression instead of
  `--rebuild`
- Prefer nix-provided Python deps over pip/venv when packaging or scripting.
- Inside nix-shell/nix develop: locate headers/libs/tools via env vars (e.g.
  `env | rg /nix/store`, `$NIX_CFLAGS_COMPILE`, `$PKG_CONFIG_PATH`,
  `$buildInputs`) rather than guessing system paths.
- My nix.conf has remote builders for aarch64-linux/aarch64-darwin/x86_64-linux
  by default. For NixOS tests on macOS, target x86_64-linux (offloaded to remote
  builder).
- Cross-arch builds: `nix-build --eval-system x86_64-linux`. Flakes: use system
  attr directly (e.g., `.#packages.x86_64-linux.hello`).
- Use nix-locate to find packages by path, e.g. `nix-locate bin/ip`
- Use `nix run` to execute applications that are not installed.
- Use `nix eval` instead of `nix flake show` to look up attributes in a flake.
- Generate/Update patch files for packages:
  1. git clone
  2. Optional: apply existing patch
  3. Apply edits
  4. Use `git format-patch` for a new patch
- `nix flake check` runs too slow. Instead, build individual tests.

## Code Quality & Testing

- Practice red-green TDD. For bugfixes this means: write failing regression test
  first.
- In flakes: format code with `flake-fmt`
- Write shell scripts that pass `shellcheck`.
- Write Python code for 3.13 that conforms to `ruff format`, `ruff check` and
  `mypy`
- Add debug output or unit tests when troubleshooting, e.g. `dbg!()` in Rust
- Tests use realistic inputs/outputs that exercise actual code, not mocks.
- Linter reports dead code: remove it.
- Linter errors: fix root cause, do not suppress warnings.
- Code comments: explain WHY, not WHAT. Describe current state, not what was
  removed.

## Git

- Commit messages: Linux-kernel style, explain WHY the change is needed.
- Always test/lint/format before committing.
- Use `gh` for GitHub (CI logs, issues, PRs), e.g.
  `gh run view 18256703410 --log`
- Use `tea` for Gitea, e.g. `tea pr 5519 --comments`

## Running programs

- CRITICAL: ALWAYS use pueue for ANY command that might take longer than 10
  seconds to avoid timeouts. This includes but is not limited to:
  - `nix build` commands
  - `merge-when-green`
  - Any test runs that might be slow
  - Any build operations (make, ninja, cargo)

  To run and wait (quote the entire command to preserve argument quoting):
  ```bash
  id=$(pueue add --print-task-id -- 'command arg1 "arg with spaces"')
  pueue follow "$id" # stream output, blocks until task finishes
  pueue log --lines 50 "$id" # Get exit status and logs
  ```

## Search

- Recommended: Use GitHub code search to find examples for libraries and APIs:
  `gh search code "foo lang:nix"`.
- Prefer cloning source code over web searches for more accurate results.
  Various projects are available in `~/git`, including: `nixpkgs`, `linux`,
  `nix`
