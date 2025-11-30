Push changes and monitor CI until completion using merge-when-green:

1. Run `merge-when-green` via pueue to avoid timeouts (waits by default)
2. Use `--message` flag to provide PR title without opening editor
3. Wait for completion and check logs
4. If CI fails, fix the issues, amend commit, and retry

## Steps

```bash
pueue add -- merge-when-green --message 'feat: your PR title here'
```

Capture the task ID from output, then wait and check logs:

```bash
pueue wait <task-id> && pueue log <task-id>
```

## On CI Failure

If CI reports errors:
1. Read the pueue log output to identify failures
2. Use `gh run view <run-id> --log-failed` or `buildbot-pr-check <pr-url>` for details
3. Fix the issues in the code
4. Amend the commit: `git add -u && git commit --amend --no-edit`
5. Run merge-when-green again
