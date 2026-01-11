# Browser CLI

A command-line interface for controlling Firefox through WebExtensions API.
Optimized for LLM agents with limited context windows.

## Overview

Browser CLI consists of three components:

1. **Firefox Extension** - Executes commands in the browser and provides visual
   feedback
2. **Native Messaging Bridge** - Facilitates communication between the CLI and
   extension
3. **CLI Client** - Minimal command-line tool that executes JavaScript via stdin

## Installation

### For Nix Users

```bash
nix run github:Mic92/dotfiles#browser-cli -- --help
```

### Manual Installation

1. **Install the Firefox Extension**
   - Open Firefox
   - Navigate to `about:debugging`
   - Click "This Firefox"
   - Click "Load Temporary Add-on"
   - Select `manifest.json` from the `extension` directory

2. **Install Native Messaging Host**
   ```bash
   browser-cli --install-host
   ```

## Usage

### Basic Usage

```bash
# List managed tabs
browser-cli --list

# Open a page
browser-cli <<'EOF'
await tab("https://example.com")
EOF

# Get page snapshot
browser-cli <<'EOF'
snap()
EOF

# Execute in a specific tab
browser-cli abc123 <<'EOF'
snap()
EOF
```

### JavaScript API

The API is optimized for token efficiency. Use `snap()` to get page state.

#### Snapshot Output Format

```
Page: Example Site
URL: https://example.com

[1] heading "Welcome"
[2] input[email] "Email" [required]
[3] input[password] "Password" [required]
[4] checkbox "Remember me"
[5] button "Sign In"
[6] link "Forgot password?"
```

- `[N]` - Reference number for use with click(), type(), etc.
- Role and accessible name shown
- Attributes in brackets: `[disabled]`, `[checked]`, `[required]`, etc.
- Input values shown as `value="..."`

#### Element Interaction (using refs)

```bash
# Get snapshot first to see available refs
browser-cli <<'EOF'
snap()
EOF

# Use refs from snapshot
browser-cli <<'EOF'
await click(1)                    // Click element [1]
await click(1, {double: true})    // Double click
await type(2, "user@example.com") // Type into element [2]
await type(2, "new", {clear: true}) // Clear first, then type
await hover(3)                    // Hover over element [3]
await drag(4, 5)                  // Drag from [4] to [5]
await select(6, "option-value")  // Select dropdown option
EOF

# Can still use CSS selectors when needed
browser-cli <<'EOF'
await click("#submit-button")
await click("Sign In", "text")
await type("input[name='email']", "user@example.com")
EOF
```

#### Keyboard

```bash
browser-cli <<'EOF'
key("Enter")
key("Tab")
key("Escape")
EOF
```

#### Page Inspection

```bash
# Get snapshot with all interactive elements
browser-cli <<'EOF'
snap()
EOF

# Filtered snapshots
browser-cli <<'EOF'
snap({forms: true})       // Only form elements
snap({links: true})       // Only links
snap({buttons: true})     // Only buttons
snap({text: "login"})     // Elements containing "login"
snap({near: 3})           // Elements near ref [3]
EOF

# Console logs
browser-cli <<'EOF'
logs()
EOF
```

#### Waiting

```bash
browser-cli <<'EOF'
await wait(1000)              // Wait 1 second
await wait("text", "Success") // Wait for text to appear
await wait("gone", "Loading") // Wait for text to disappear
EOF
```

#### Screenshots

```bash
browser-cli <<'EOF'
await shot()                  // Screenshot, returns data URL
await shot("/tmp/page.png")   // Screenshot to specific path
EOF
```

#### Tab Management

```bash
browser-cli <<'EOF'
await tab()                      // Create new tab
await tab("https://example.com") // Create with URL
await tabs()                     // List all tabs
EOF
```

### Examples

```bash
# Search on Google
browser-cli <<'EOF'
await tab("https://google.com")
snap()
EOF
# Output shows [12] combobox "Suche"

browser-cli <<'EOF'
await type(12, "hello world")
diff()
EOF
# Shows: Added (autocomplete options), Changed (input value)

# Form filling with refs
browser-cli <<'EOF'
await tab("https://example.com/login")
snap()
EOF
# Output: [1] input "Email", [2] input "Password", [3] button "Sign In"

browser-cli <<'EOF'
await type(1, "user@test.com")
await type(2, "secret123")
await click(3)
diff()
EOF

# Wait for dynamic content
browser-cli <<'EOF'
await click(5)
await wait("text", "Success")
snap()
EOF

# Take a screenshot
browser-cli <<'EOF'
await shot("/tmp/page.png")
EOF
```

## Architecture

```
┌─────────────┐     Unix Socket     ┌──────────────┐     Native      ┌────────────┐
│    CLI      │ ◄─────────────────► │    Bridge    │ ◄─────────────► │ Extension  │
│  (stdin)    │                     │   Server     │    Messaging    │            │
└─────────────┘                     └──────────────┘                 └────────────┘
```

## API Reference

### Interaction

| Function                         | Description     |
| -------------------------------- | --------------- |
| `click(ref)`                     | Click element   |
| `click(ref, {double: true})`     | Double click    |
| `type(ref, text)`                | Type into input |
| `type(ref, text, {clear: true})` | Clear first     |
| `key(name)`                      | Press key       |
| `hover(ref)`                     | Hover element   |
| `drag(from, to)`                 | Drag and drop   |
| `select(ref, value)`             | Select option   |

### Inspection

| Function      | Description          |
| ------------- | -------------------- |
| `snap()`      | Get page snapshot    |
| `snap({...})` | Filtered snapshot    |
| `diff()`      | Diff since last snap |
| `logs()`      | Get console logs     |
| `find(ref)`   | Find DOM element     |

### Waiting

| Function            | Description               |
| ------------------- | ------------------------- |
| `wait(ms)`          | Wait milliseconds         |
| `wait("idle")`      | Wait for DOM to stabilize |
| `wait("text", str)` | Wait for text             |
| `wait("gone", str)` | Wait for text gone        |

### Media

| Function              | Description            |
| --------------------- | ---------------------- |
| `shot()`              | Screenshot             |
| `shot(path)`          | Screenshot to path     |
| `download(url)`       | Download file          |
| `download(url, name)` | Download with filename |

### Tabs

| Function   | Description      |
| ---------- | ---------------- |
| `tab()`    | New tab          |
| `tab(url)` | New tab with URL |
| `tabs()`   | List tabs        |

## Development

### Project Structure

```
browser-cli/
├── extension/          # Firefox WebExtension
│   ├── manifest.json
│   ├── background.js   # Extension service worker
│   └── content.js      # Page automation and JS API
├── browser_cli/        # Python CLI package
│   ├── cli.py          # CLI entry point
│   ├── client.py       # Unix socket client
│   ├── bridge.py       # Native messaging bridge
│   └── server.py       # Bridge server
└── pyproject.toml
```

### Building

For Nix users:

```bash
nix build .#browser-cli
```

## Troubleshooting

### Extension Not Connecting

1. Ensure Firefox is running
2. The Browser CLI extension is installed
3. Native messaging host is installed: `browser-cli --install-host`
4. Check Firefox console for errors: `Ctrl+Shift+J`

### Commands Timing Out

- Use `wait()` for dynamic content: `await wait("text", "Loaded")`
- Check element refs are current: `snap()` to refresh

### Stale Refs

Refs are reset on each snapshot. If you get "Element [N] not found", call
`snap()` to get fresh refs.

## License

This project is part of the dotfiles repository and follows its licensing terms.
