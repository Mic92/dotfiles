- Use `--log-format bar-with-logs` with Nix for improved build log output.
- Add new untracked files in Nix flakes with `git add`.
- Tools that can be used: fd, rg, dnsutils, lsof
- Format code with `flake-fmt` if the current project has a flake with a
  formatter defined.
- Write shell scripts that pass `shellcheck`.
- Write Python code for 3.13 that conforms to `ruff format`, `ruff check` and
  `mypy`
- Use `$HOME/.claude/outputs` as a scratch directory.
- Always test/lint/format your code before committing.
- Add debug output or unit tests when troubleshooting.
- Avoid mocking in tests.
- IMPORTANT!!! Use pueue for commands longer than 10 seconds to avoid
  timeouts. To run and wait: `task_id=$(pueue add -- command | grep -oE '[0-9]+'); pueue wait "$task_id"`
- Use strace/sysdig/bcc on Linux and dtrace on macOS for debugging
- Use tmux when trying to interact with interactive cli/tuis
- Use the gh tool to interact with GitHub.
- Use the tea CLI tool to interact with Gitea.
- Use nix-locate to find packages by file path.
- Use nix run to execute applications that are not installed.
- Recommended: Use GitHub code search to find examples for the latest APIs:
  `gh search code "foo lang:nix"`.
- Use `nix eval` instead of `nix flake show` to look up attributes in a flake.
- Do not use `nix flake check` on the whole flake; it is too slow. Instead,
  build individual tests.
- Prefer cloning source code over web searches. Various projects are available
  in `$HOME/git`, including:
  - `$HOME/git/nixpkgs`
  - `$HOME/git/linux`
  - `$HOME/git/nix`
  - `$HOME/work/clan/clan-core`
- Please use Kagi to search the web instead of Brave Search!!!! The search
  results are better.
  - `kagi-search "nixpkgs buildPythonPackage examples"`
  - `kagi-search -j "nix flake inputs follows" | jq -r '.[0].url'`
- avoid using cd, instead use absolute paths when possible to avoid: Error: cd
  to '/foo' was blocked. For security, Claude Code may only change directories
  to child directories of the allowed working directories.
