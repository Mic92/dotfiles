# Browser CLI

A command-line interface for controlling Firefox through WebExtensions API.
Features visual cursor animations and comprehensive browser automation
capabilities.

## Overview

Browser CLI consists of three components:

1. **Firefox Extension** - Executes commands in the browser and provides visual
   feedback
2. **WebSocket Bridge Server** - Facilitates communication between the CLI and
   extension
3. **CLI Client** - Command-line tool for sending automation commands

## Features

- **Visual Cursor** - Animated red cursor shows where actions are performed
- **Element Selection** - Find elements by CSS selector, text content,
  aria-label, or placeholder
- **Browser Automation** - Navigate, click, type, hover, drag & drop, select
  options, press keys
- **Screenshot Capture** - Take screenshots of the current page
- **Console Logs** - Retrieve JavaScript console output
- **ARIA Snapshots** - Get accessibility tree representation of the page
- **On-Demand Activation** - Extension only runs on tabs where explicitly
  enabled

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
   browser-cli install-host
   ```

3. **Start the WebSocket Bridge Server**
   ```bash
   browser-cli-server
   ```

   Or with nix:

   ```
   nix shell github:Mic92/dotfiles#browser-cli --command browser-cli-server
   ```

## Usage

### Enable Extension on a Tab

1. Navigate to the desired webpage
2. Right-click and select "Enable Browser CLI on this tab"
3. A blue banner will appear at the top indicating the extension is active
4. Click the X button on the banner to disable

### CLI Commands

```bash
# Navigation
browser-cli navigate https://example.com
browser-cli back
browser-cli forward

# Interaction
browser-cli click "Sign In"
browser-cli type "input[name='email']" "user@example.com"
browser-cli hover "button.submit"
browser-cli select "select#country" "United States"
browser-cli key "Enter"

# Drag and Drop
browser-cli drag ".draggable-item" ".drop-zone"

# Information
browser-cli screenshot output.png
browser-cli console
browser-cli snapshot
```

### Element Selection

Elements can be found using:

- **CSS Selectors**: `browser-cli click "#submit-button"`
- **Text Content**: `browser-cli click "Sign In"`
- **ARIA Labels**: `browser-cli click "Close dialog"`
- **Placeholders**: `browser-cli type "Enter your email" "user@example.com"`

## Architecture

```
┌─────────────┐     WebSocket      ┌──────────────┐     Native      ┌────────────┐
│   CLI       │ ◄─────────────────► │    Bridge    │ ◄─────────────► │ Extension  │
│ (Port 9223) │                     │   Server     │    Messaging    │(Port 9222) │
└─────────────┘                     └──────────────┘                  └────────────┘
```

## Development

### Project Structure

```
browser-cli/
├── extension/          # Firefox WebExtension
│   ├── manifest.json
│   ├── background.js   # Extension service worker
│   └── content.js      # Page automation and cursor
├── browser_cli/        # Python CLI package
│   ├── __init__.py
│   ├── cli.py          # CLI entry point
│   ├── client.py       # WebSocket client
│   ├── server.py       # Bridge server
│   ├── commands.py     # Command definitions
│   └── errors.py       # Custom exceptions
└── pyproject.toml      # Python package configuration
```

### Setup Development Environment

```bash
# Install JavaScript dependencies
cd extension
npm install

# Install Python dependencies (if not using Nix)
cd ..
pip install -e ".[dev]"
```

### Available NPM Scripts

```bash
cd extension

# Type checking with TypeScript
npm run typecheck

# Lint JavaScript code
npm run lint

# Fix linting issues automatically
npm run lint:fix

# Run extension in Firefox (development mode)
npm start

# Build extension package
npm run build
```

### Python Development

```bash
# Lint Python code
ruff check .

# Fix Python linting issues
ruff check --fix .

# Format Python code
ruff format .

# Type check Python code
mypy .
```

### Building

For Nix users:

```bash
nix build .#browser-cli
```

## Troubleshooting

### Extension Not Connecting

1. Ensure the WebSocket bridge server is running: `browser-cli-server`
2. Check that the extension is enabled on the current tab
3. Verify native messaging host is installed: `browser-cli install-host`
4. Check Firefox console for errors: `Ctrl+Shift+J`

### Commands Timing Out

- Some pages may have slow-loading elements
- Try increasing wait time between commands
- Ensure elements exist before interacting: `browser-cli snapshot`

### Permission Errors

The extension requires permissions for:

- `activeTab` - Interact with the current tab
- `tabs` - Navigate and manage tabs
- `contextMenus` - Add right-click menu options
- `nativeMessaging` - Communicate with CLI
- `storage` - Save extension state

## License

This project is part of the dotfiles repository and follows its licensing terms.
