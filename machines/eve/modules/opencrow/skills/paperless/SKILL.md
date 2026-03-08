---
name: paperless
description: Manage documents in Paperless-ngx (search, upload, tag). Use for document management tasks.
---

# Paperless-ngx

Configuration is pre-set. No delete permissions — use view/add/change only.

## Documents

```bash
paperless-cli documents search "invoice"               # search
paperless-cli documents search --tags "receipts,tax"    # filter by tags
paperless-cli documents get <id>                        # details
paperless-cli documents get <id> --metadata             # metadata
paperless-cli documents get <id> --download             # download
paperless-cli documents get <id> --download --original  # download original
paperless-cli documents upload file.pdf --title "Title" --tags "tag1,tag2"
paperless-cli documents update <id> --add-tags "tax" --remove-tags "inbox"
paperless-cli documents bulk 1 2 3 --add-tags "processed"
```

## Tags

```bash
paperless-cli tags list
paperless-cli tags create "Tag Name" --color "#FF0000"
```
