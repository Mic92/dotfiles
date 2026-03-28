---
name: github
description: Read-only GitHub API access as Mic92 — issues, PRs, releases, commits, actions runs, etc.
---

```bash
n8n-hooks github <path> [-q k=v ...]     # GET api.github.com<path>
n8n-hooks github discover <term>         # grep endpoints
n8n-hooks github discover /<path>        # exact path → show params
```

GET-only (enforced server-side). Output is JSON — pipe through `jq`. Pagination:
`-q per_page=N -q page=M` (max 100).

Example:

```bash
n8n-hooks github /repos/NixOS/nixpkgs/pulls/12345/files | jq '.[].filename'
```
