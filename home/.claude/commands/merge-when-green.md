Push changes and monitor CI until completion using merge-when-green:

1. Run `merge-when-green --wait` via pueue to avoid timeouts
2. Use `--message` flag to provide PR title without opening editor
3. Monitor CI status until all checks pass or fail
4. Check GitHub -> Buildbot for potential CI errors if needed with gh

Example:
```bash
pueue add -- merge-when-green --wait --message 'feat: your PR title here'
# Get task ID and wait for completion
pueue status
pueue wait <task-id> && pueue log <task-id>
```

The `--wait` flag will continuously check CI status and report when all checks pass or fail.
