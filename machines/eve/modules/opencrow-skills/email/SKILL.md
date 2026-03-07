---
name: email
description: Read and inspect flagged/starred emails from the Maildir using mblaze.
---

# Email Access

Flagged emails live in `/var/mail/flagged/` (read-only Maildir). Use **mblaze**:

```bash
# List messages
mscan /var/mail/flagged/{cur,new}/

# Show a message (renders MIME)
mshow /var/mail/flagged/cur/<filename>

# Extract headers
mhdr -h From -h Subject -h Date /var/mail/flagged/cur/<filename>

# List/extract MIME parts
mshow -t <file>    # list parts
mshow -x <file>    # extract attachments

# Search by header
mpick -t 'from =~ "someone"' /var/mail/flagged/{cur,new}/

# Thread and list
mthread /var/mail/flagged/{cur,new}/ | mscan
```
