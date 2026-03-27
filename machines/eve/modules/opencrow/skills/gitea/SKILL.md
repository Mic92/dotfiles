---
name: gitea
description: Access git.thalheim.io (Gitea) API as Janet — create repos, issues, PRs, releases, etc.
---

```bash
n8n-hooks gitea <GET|POST|PUT|PATCH|DELETE> <path> [-d '<json>'|@file|-] [-q k=v ...]
n8n-hooks gitea discover <term>     # grep endpoints
n8n-hooks gitea discover /<path>    # exact path → params + body schema
```
