# Paperless-ngx CLI

A command-line interface for managing Paperless-ngx documents, mail accounts,
mail rules, and tags.

## Installation

```bash
nix run github:Mic92/dotfiles#paperless-cli
```

## Configuration

Create a configuration file at `~/.config/paperless-cli/config.json`:

```json
{
  "url": "https://paperless.example.com",
  "token_command": "rbw get paperless-api-token"
}
```

### Configuration Options

- `url`: The base URL of your Paperless-ngx instance
- `token_command`: A command that outputs the API token (e.g., from a password
  manager)

## Usage

### Documents

#### Search documents

```bash
# Search all documents
paperless-cli documents search

# Search with query
paperless-cli documents search "invoice"

# Search with pagination
paperless-cli documents search --page 2 --page-size 50
```

#### Get document details

```bash
# Show document details
paperless-cli documents get <document-id>

# Show document metadata
paperless-cli documents get <document-id> --metadata

# Download document
paperless-cli documents get <document-id> --download

# Download original (if available)
paperless-cli documents get <document-id> --download --original

# Download with custom filename
paperless-cli documents get <document-id> --download -o output.pdf
```

#### Upload document

```bash
# Upload a document
paperless-cli documents upload /path/to/document.pdf

# Upload with title
paperless-cli documents upload /path/to/document.pdf --title "My Document"

# Upload with tags (comma-separated tag IDs)
paperless-cli documents upload /path/to/document.pdf --tags "1,2,3"
```

#### Delete document

```bash
# Delete with confirmation
paperless-cli documents delete <document-id>

# Delete without confirmation
paperless-cli documents delete <document-id> --force
```

### Tags

#### List tags

```bash
paperless-cli tags list
```

#### Create tag

```bash
# Create a tag
paperless-cli tags create "Tag Name"

# Create with color
paperless-cli tags create "Tag Name" --color "#FF0000"
```

#### Delete tag

```bash
# Delete with confirmation
paperless-cli tags delete <tag-id>

# Delete without confirmation
paperless-cli tags delete <tag-id> --force
```

### Mail Accounts

#### List mail accounts

```bash
paperless-cli mail-accounts list
```

### Mail Rules

#### List mail rules

```bash
paperless-cli mail-rules list
```

#### Show mail rule details

```bash
paperless-cli mail-rules show <rule-id>
```

#### Create a mail rule

```bash
paperless-cli mail-rules create "Rule Name" \
  --filter-from "sender@example.com" \
  --assign-tags "1,2,3"
```

#### Update a mail rule

```bash
paperless-cli mail-rules update <rule-id> \
  --name "New Name" \
  --filter-subject "Invoice"
```

#### Delete a mail rule

```bash
paperless-cli mail-rules delete <rule-id>
```

## Environment Variables

- `PAPERLESS_URL`: Base URL of your Paperless-ngx instance
- `PAPERLESS_TOKEN`: API token for authentication
