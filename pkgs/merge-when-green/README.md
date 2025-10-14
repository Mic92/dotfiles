# merge-when-green

Automated PR creation and merging for GitHub and Gitea with CI monitoring.

## Features

- 🚀 **Automatic PR Creation** - Creates pull requests from your current branch
  with commit messages as description
- ♻️ **Smart PR Detection** - Detects and reuses existing PRs instead of failing
- 🔄 **Auto-merge** - Enables auto-merge and waits for CI checks to complete
- 📊 **Real-time CI Monitoring** - Live status updates with colored output
  showing passed/failed/pending checks
- 🔍 **Buildbot Integration** - Automatically runs `buildbot-pr-check` when
  checks fail (if available)
- 🎯 **Multi-platform** - Supports both GitHub and Gitea
- 🎨 **Code Formatting** - Runs `flake-fmt` and attempts to fix issues with
  `git-absorb`
- 🔁 **Auto-rebase** - Rebases your branch after successful merge

## Usage

```bash
# Create PR and wait for CI to pass
merge-when-green

# Create PR but don't wait for CI
merge-when-green --no-wait

# Provide custom PR title and body
merge-when-green -m "Fix bug in auth\n\nThis fixes the authentication issue"
```

## Requirements

### GitHub

- `gh` - GitHub CLI tool
- Repository with auto-merge enabled

### Gitea

- `tea` - Gitea CLI tool
- `GITEA_TOKEN` environment variable (for auto-merge)

### Optional

- `flake-fmt` - For code formatting
- `git-absorb` - For automatic commit fixing
- `lazygit` - Interactive git UI (used when formatting fails)
- `buildbot-pr-check` - For detailed CI failure information

## Workflow

1. **Prepare Repository**
   - Pulls latest changes from default branch
   - Runs `flake-fmt` to check code formatting
   - If formatting fails, attempts to fix with `git-absorb`

2. **Create/Update PR**
   - Checks if PR already exists for the branch
   - If exists: enables auto-merge and continues
   - If new: opens editor with commit messages to create PR description

3. **Monitor CI** (unless `--no-wait`)
   - **GitHub**: Shows real-time check status with colored output
     ```
     [19:14:01] Checks - Passed: 5, Failed: 0, Pending: 3
     [19:14:11] Checks - Passed: 8, Failed: 0, Pending: 0
     ✓ PR successfully merged!
     ```
   - **Gitea**: Simple polling every 30 seconds
   - Automatically runs `buildbot-pr-check` when checks fail

4. **Finalize**
   - Waits for PR to be merged via auto-merge
   - Fetches latest changes
   - Rebases your branch onto the updated default branch

## Output Examples

### Successful Merge

```
Detected GitHub
Getting repository information...
Target branch: main

Preparing changes...
✓ Code formatting check passed

Pushing changes...

✓ Using existing pull request
Enabling auto-merge...
✓ Auto-merge enabled

Waiting for PR 'my-feature' to merge...
[19:14:01] Checks - Passed: 3, Failed: 0, Pending: 5
[19:14:11] Checks - Passed: 6, Failed: 0, Pending: 2
[19:14:21] Checks - Passed: 8, Failed: 0, Pending: 0

✓ PR successfully merged!
✓ Rebased onto latest changes
```

### Failed Checks

```
[19:14:01] Checks - Passed: 5, Failed: 2, Pending: 0

🔍 Running buildbot-pr-check to get detailed failure information...
[buildbot output here]

✗ 2 checks failed
```

## Platform Detection

The tool automatically detects whether you're using GitHub or Gitea by checking:

1. GitHub: `gh repo view` command
2. Gitea: `tea repos list` command
3. Falls back to GitHub if neither works

## Configuration

### Gitea Auto-merge

For Gitea, set the `GITEA_TOKEN` environment variable:

```bash
export GITEA_TOKEN="your-token-here"
```

### Editor

The tool uses `$EDITOR` (defaults to `vim`) to edit PR descriptions:

```bash
export EDITOR="nano"
```

## Exit Codes

- `0` - Success
- `1` - Error (formatting failed, no changes, PR closed, etc.)
- `130` - Interrupted by user (Ctrl+C)

## Notes

- If on the default branch, creates a new branch named `merge-when-green-$USER`
- Formatting issues open `lazygit` in interactive mode (if available)
- PR title/body defaults to aggregated commit messages from your branch
- Checks run every 10 seconds for GitHub, 30 seconds for Gitea
