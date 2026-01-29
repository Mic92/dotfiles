---
description: Launch tasks in new git worktrees using workmux
---

Launch one or more tasks in new git worktrees using workmux.

Tasks: $ARGUMENTS

## Instructions

Note: The tasks above may reference something discussed earlier in the
conversation (e.g., "do option 2", "implement the fix we discussed"). Include
all relevant context from the conversation in each prompt you write.

If tasks reference a markdown file (e.g., a plan or spec), re-read the file to
ensure you have the latest version before writing prompts.

For each task:

1. Generate a short, descriptive worktree name (2-4 words, kebab-case)
2. Write a detailed implementation prompt to a temp file
3. Run `workmux add <worktree-name> -b -P <temp-file>` to create the worktree

The prompt file should:

- Include the full task description
- Use RELATIVE paths only (never absolute paths, since each worktree has its own
  root directory)
- Be specific about what the agent should accomplish

## Workflow

Write ALL temp files first, THEN run all workmux commands in parallel.

After creating the worktrees, inform the user which branches were created.
