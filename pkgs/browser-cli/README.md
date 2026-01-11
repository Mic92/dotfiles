# Browser CLI

A command-line interface for controlling Firefox through WebExtensions API.
Exposes a JavaScript API for browser automation, similar to how pexpect-cli
works for terminal automation.

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
# Execute JavaScript (reads from stdin)
echo 'document.title' | browser-cli

# Execute in a specific tab
echo 'navigate("https://example.com")' | browser-cli abc123

# List managed tabs
browser-cli --list
```

### JavaScript API

The CLI executes JavaScript with the following API functions available:

#### Navigation
```javascript
await navigate("https://example.com")  // Navigate to URL
await back()                            // Go back in history
await forward()                         // Go forward in history
```

#### Element Interaction
```javascript
// Click element (selector types: css, text, aria-label, placeholder)
await click("#submit-button")
await click("Sign In", "text")
await click("Close dialog", "aria-label")

// Type into input
await type("input[name='email']", "user@example.com")
await type("Enter your email", "hello", "placeholder")

// Hover over element
await hover(".dropdown-trigger")

// Drag and drop
await drag(".draggable", ".dropzone")

// Select dropdown option
await select("#country", "United States")

// Press keyboard key
key("Enter")
key("Tab")
key("Escape")
```

#### Page Information
```javascript
// Get accessibility tree snapshot
snapshot()
snapshot(0, 100)  // With pagination: offset, limit

// Get console logs
getConsole()

// Find element (returns DOM element)
findElement("#my-id")
findElement("Click me", "text")
```

#### Screenshots
```javascript
await screenshot()                  // Save to screenshot.png
await screenshot("/tmp/page.png")   // Save to specific path
```

#### Tab Management
```javascript
const tabId = await newTab()                    // Create new tab
const tabId = await newTab("https://example.com")  // Create with URL
await closeTab()                                // Close current tab
await closeTab("abc123")                        // Close specific tab
const tabs = await listTabs()                   // List all tabs
```

### Examples

```bash
# Complex automation with heredoc
browser-cli <<'EOF'
await navigate("https://google.com");
await type("textarea[name='q']", "hello world");
key("Enter");
EOF

# Create a new tab and automate it
browser-cli <<'EOF'
const tabId = await newTab("https://example.com");
console.log("Created tab:", tabId);
await click("a");
EOF

# Get page title
echo 'document.title' | browser-cli

# Get all links on the page
browser-cli <<'EOF'
Array.from(document.querySelectorAll('a'))
  .map(a => ({ href: a.href, text: a.textContent.trim() }))
  .filter(a => a.href && a.text)
EOF

# Take a screenshot
echo 'await screenshot("/tmp/page.png")' | browser-cli
```

## Architecture

```
┌─────────────┐     Unix Socket     ┌──────────────┐     Native      ┌────────────┐
│    CLI      │ ◄─────────────────► │    Bridge    │ ◄─────────────► │ Extension  │
│  (stdin)    │                     │   Server     │    Messaging    │            │
└─────────────┘                     └──────────────┘                 └────────────┘
```

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

- Some pages may have slow-loading elements
- Try adding delays: `await new Promise(r => setTimeout(r, 1000))`
- Ensure elements exist: `snapshot()` to see the page structure

## License

This project is part of the dotfiles repository and follows its licensing terms.
