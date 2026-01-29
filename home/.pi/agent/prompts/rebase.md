---
description: Rebase current branch on main, origin, or specified branch
---

Rebase the current branch.

Arguments: $ARGUMENTS

Behavior:

- No arguments: rebase on local main
- "origin": fetch origin, rebase on origin/main
- "origin/branch": fetch origin, rebase on origin/branch
- "branch": rebase on local branch

Steps:

1. Parse arguments:
   - No args → target is "main", no fetch
   - Contains "/" (e.g., "origin/develop") → split into remote and branch, fetch
     remote, target is remote/branch
   - Just "origin" → fetch origin, target is "origin/main"
   - Anything else → target is that branch name, no fetch
2. If fetching, run: `git fetch <remote>`
3. Run: `git rebase <target>`
4. If conflicts occur, handle them carefully (see below)
5. Continue until rebase is complete

Handling conflicts:

- BEFORE resolving any conflict, understand what changes were made to each
  conflicting file in the target branch
- For each conflicting file, run `git log -p -n 3 <target> -- <file>` to see
  recent changes to that file in the target branch
- The goal is to preserve BOTH the changes from the target branch AND our
  branch's changes
- After resolving each conflict, stage the file and continue with
  `git rebase --continue`
- If a conflict is too complex or unclear, ask for guidance before proceeding
