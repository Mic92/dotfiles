## General Guidelines

- Follow XDG desktop standards
- Regularly reason about security implications of the code
- Use `$HOME/.claude/outputs` as a scratch directory.
- In the Bash tool use absolute paths over `cd`

## Available Tools

- fd, rg, dnsutils, lsof, gdb, binutils, ast-grep, graphicsmagic (gm)
- On Linux: strace/sysdig/bcc
- macOS: dtrace

## Nix-specific

- Use `--log-format bar-with-logs` with Nix for improved build log output.
- Add new untracked files in Nix flakes with `git add`.
- Prefer nix to fetch python dependencies
- When looking for build dependencies in a nix-shell/nix develop, check
  environment variables for store paths to find the correct dependency versions.
- On nix build failures, filter nix log output for the root cause instead of
  time-intense rebuilding.
- Use nix-locate to find packages by path. i.e. `nix-locate bin/ip`
- Use `nix run` to execute applications that are not installed.
- Use `nix eval` instead of `nix flake show` to look up attributes in a flake.
- Do not use `nix flake check` on the whole flake; it is too slow. Instead,
  build individual tests.

## Code Quality & Testing

- Format code with `flake-fmt` if the current project has a flake with a
  formatter defined.
- Write shell scripts that pass `shellcheck`.
- Write Python code for 3.13 that conforms to `ruff format`, `ruff check` and
  `mypy`
- Add debug output or unit tests when troubleshooting.
- When writing test use realistic inputs/outputs that test the actual code as
  opposed to mocked out versions
- IMPORTANT: GOOD: When given a linter error, address the root cause of the
  linting error. BAD: silencing lint errors. Exhaustivly fix all linter errors.

## Git

- When writing commit messages/comments focus on the WHY rather than the WHAT.
- Always test/lint/format your code before committing.
- Use the gh tool to interact with GitHub i.e.: `gh run view 18256703410 --log`
- Use the tea CLI tool to interact with Gitea.

## Performance

- CRITICAL: ALWAYS use pueue for ANY command that might take longer than 10
  seconds to avoid timeouts. This includes but is not limited to:
  - `clan machines update` (deployment commands)
  - `nix build` commands
  - `merge-when-green`
  - Any test runs that might be slow
  - Any deployment or build operations (make, ninja, cargo)

  To run and wait:
  ```bash
  id=$(pueue add -- command | grep -oE '[0-9]+'); pueue wait "$id"; pueue log "$id"
  ```

## Search

- Recommended: Use GitHub code search to find examples for libraries and APIs:
  `gh search code "foo lang:nix"`.
- Prefer cloning source code over web searches for more accurate results.
  Various projects are available in `$HOME/git`, including:
  - `$HOME/git/nixpkgs`
  - `$HOME/git/linux`
  - `$HOME/git/nix`
  - `$HOME/work/clan/clan-core`
- Use Kagi instead of the Websearch tool for better search results:
  - `kagi-search "nixpkgs buildPythonPackage examples"`
  - `kagi-search -j "nix flake inputs follows" | jq -r '.[0].url'`
