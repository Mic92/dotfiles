# n8n-hooks

CLI to invoke n8n webhooks. Currently supports storing email drafts in IMAP.

## Usage

```
n8n-hooks store-draft --to "a@b.com" --subject "Hi" --body-plain "Hello"
```

## Adding a new hook

1. Create `n8n_hooks/hooks/<name>.py` with `register(subparsers)` and
   `run(args, config)`
2. Import and call `register()` in `cli.py`
3. Add the hook key to the config JSON
