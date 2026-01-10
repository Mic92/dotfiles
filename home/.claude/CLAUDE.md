## Available Tools

- fd, rg, dnsutils, lsof, gdb, binutils, graphicsmagic (gm)
- On Linux: strace/sysdig/bcc
- macOS: strace (this was ported)

## General Guidelines

- Follow XDG desktop standards when writing code
- Use `$HOME/.claude/outputs` as a scratch directory.

## Nix-specific

- Use `nix log /nix/store/xxxx | grep <key-word>` to inspect failed nix builds
- Add new untracked files in Nix flakes with `git add`.
- To get a rebuild of a nix package change the nix expression instead of
  `--rebuild`
- Prefer nix to fetch python dependencies
- When looking for build dependencies in a nix-shell/nix develop, check
  environment variables for store paths to find the correct dependency versions.
- My nix.conf has remote builders for aarch64-linux/aarch64-darwin/x86_64-linux
  by default, for NixOS tests. Therefore, use x86_64-linux on macOS machines
- Cross-arch builds: `nix-build --eval-system x86_64-linux`. Flakes: use system
  attr directly (e.g., `.#packages.x86_64-linux.hello`).
- Use nix-locate to find packages by path. i.e. `nix-locate bin/ip`
- Use `nix run` to execute applications that are not installed.
- Use `nix eval` instead of `nix flake show` to look up attributes in a flake.
- Generate/Update patch files for packages:
  1. git clone
  2. Optional: apply existing patch
  3. Apply edits
  4. Use `git format-patch` for a new patch
- `nix flake check` runs too slow. Instead, build individual tests.

## Code Quality & Testing

- practice TDD
- In flakes: format code with `flake-fmt`
- Write shell scripts that pass `shellcheck`.
- Write Python code for 3.13 that conforms to `ruff format`, `ruff check` and
  `mypy`
- Add debug output or unit tests when troubleshooting i.e. dbg!() in Rust
- When writing test use realistic inputs/outputs that test the actual code as
  opposed to mocked out versions
- Start fixing bugs by implementing a failing regression test first.
- When a linter is detecting dead code, remove the dead code.
- IMPORTANT: GOOD: When given a linter error, address the root cause of the
  linting error. BAD: silencing lint errors. Exhaustivly fix all linter errors.

## Git

- When writing commit messages/comments focus on the WHY rather than the WHAT.
- Use kernel-mailing style commit messages
- Always test/lint/format your code before committing.
- Use the gh tool to interact with GitHub i.e.: `gh run view 18256703410 --log`
- Use the tea CLI tool to interact with Gitea i.e.: `tea pr 5519 --comments`
- To get buildbot ci logs, use buildbot-pr-check on the pull request: i.e.
  `buildbot-pr-check https://github.com/numtide/nix-ai-tools/pull/993`

## Running programs

- CRITICAL: ALWAYS use pueue for ANY command that might take longer than 10
  seconds to avoid timeouts. This includes but is not limited to:
  - `clan machines update` (deployment commands)
  - `nix build` commands
  - `merge-when-green`
  - Any test runs that might be slow
  - Any deployment or build operations (make, ninja, cargo)

  To run and wait (note: quote the entire command to preserve argument quoting):
  ```bash
  pueue add -- 'command arg1 "arg with spaces"'
  pueue wait <task-id> && pueue log <task-id>
  ```

## Search

- Recommended: Use GitHub code search to find examples for libraries and APIs:
  `gh search code "foo lang:nix"`.
- Prefer cloning source code over web searches for more accurate results.
  Various projects are available in `~/git`, including: `nixpkgs`, `linux`,
  `nix`, also see `~/work/clan/clan-core`
- Use Kagi for searching the web:
- `kagi-search "nixpkgs buildPythonPackage examples"`
- `kagi-search -j "nix flake inputs follows" | jq -r '.results[].url'`
