---
name: gitea
description: Access git.thalheim.io (Gitea) API as Janet — create repos, issues, PRs, releases, etc.
---

```bash
n8n-hooks gitea <GET|POST|PUT|PATCH|DELETE> <path> [-d '<json>'|@file|-] [-q k=v ...]
n8n-hooks gitea discover <term>     # grep endpoints
n8n-hooks gitea discover /<path>    # exact path → params + body schema
```

Janet's SSH key is registered on git.thalheim.io. Use `gitea@` as the SSH user
(not `git@`):

```bash
GIT_SSH_COMMAND="ssh -i ~/.ssh/id_ed25519_janet -o IdentitiesOnly=yes" \
  git clone gitea@git.thalheim.io:janet/dotfiles.git
```
