---
name: email
description: Read flagged emails from Maildir (mblaze) and draft replies via n8n-hooks.
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

# Drafting Emails

Use **n8n-hooks** to store drafts in IMAP for review before sending:

```bash
# Basic draft (plain text auto-wrapped in <pre> for HTML)
n8n-hooks store-draft --to "a@example.com" --subject "Hi" --body-plain "Hello"

# All options
n8n-hooks store-draft \
  --to "a@example.com" --cc "b@example.com" --bcc "c@example.com" \
  --from "other@thalheim.io" \
  --subject "Re: Thread" \
  --body-plain "text" --body-html "<p>text</p>" \
  --in-reply-to "<msgid@host>" --references "<msgid@host>" \
  --attach file.pdf

# Body from stdin
echo "body" | n8n-hooks store-draft --to "a@example.com" --subject "Hi" --body-plain -
```
