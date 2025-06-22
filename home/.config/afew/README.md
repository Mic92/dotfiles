# Afew Claude Spam Filter

This directory contains custom afew filters for email processing, including a
Claude-powered spam filter.

## Running Tests

To run the tests for the Claude spam filter:

```bash
# Enter the afew directory
cd home/.config/afew

# Run tests using nix-shell
nix-shell --run "pytest tests/ -v"

# Run a specific test
nix-shell --run "pytest tests/test_claude_spam_filter.py -v"

# Run tests with output
nix-shell --run "pytest tests/ -v -s"
```

## Project Structure

- `afew_filters/` - Package containing the filters
  - `claude_spam_filter.py` - Main Claude spam filter implementation
  - `spam_database.py` - SQLite database for tracking spam scores
  - `manage_spam_scores.py` - CLI tool for managing the spam database
- `tests/` - Test suite
  - `test_claude_spam_filter.py` - Integration tests with example emails
- `claude_spam_filter.py` - Import wrapper for afew compatibility
- `config` - Afew configuration file
- `shell.nix` - Development environment with test dependencies

## Development

The development environment is provided by `shell.nix`, which includes:

- Python 3.12
- pytest for running tests
- afew and notmuch libraries
- All filter dependencies

To enter the development shell:

```bash
nix-shell
```
