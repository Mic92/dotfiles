# merge-when-green

Push the current branch, open a PR, enable auto-merge and wait until CI is
green. Works with GitHub (`gh`) and Gitea (`tea`).

## Usage

```bash
merge-when-green              # create PR and wait for CI
merge-when-green --no-wait    # create PR, don't wait
merge-when-green -m "title"   # PR title/body from argument instead of $EDITOR
```

## Workflow

1. Rebases onto the default branch and runs `flake-fmt`. Formatting fixes are
   folded into your commits with `git-absorb`; if that fails, `lazygit` opens.
2. Pushes the branch (on the default branch, a `merge-when-green-$USER` branch
   is created) and creates a PR, or reuses an existing open one. The PR
   description is taken from your commit messages, editable via `$EDITOR`.
3. Enables auto-merge and polls check status. On failing checks it runs
   `nbo log` (nixbot CLI) to show failure logs, if installed.
4. After the merge, rebases your local branch onto the updated default branch.

## Requirements

- GitHub: `gh`, repository with auto-merge enabled
- Gitea: `tea`, `GITEA_TOKEN` set (used to enable auto-merge via API)
- Optional: `flake-fmt`, `git-absorb`, `lazygit`, `nbo`

The platform is detected by trying `gh repo view` first, then `tea repos list`.
