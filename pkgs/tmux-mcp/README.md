# Tmux MCP Server

A Model Context Protocol (MCP) server that provides tmux integration for Claude
Code.

## Features

- Run commands in tmux sessions
- Send input to running processes
- List sessions and panes
- Capture pane output
- Kill panes

## Usage

This package is typically used via the Claude Code MCP server configuration.

## Development

### Running Tests

Run tests using nix-shell:

```bash
nix-shell --run "pytest tests/ -v"
```

### Development Shell

Enter development shell:

```bash
nix-shell
```

Format code:

```bash
ruff format .
```

Lint code:

```bash
ruff check .
```
