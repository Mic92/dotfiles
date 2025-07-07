# buildbot-pr-check

Check Buildbot CI status for GitHub and Gitea pull requests.

## Features

- Supports both GitHub and Gitea pull requests
- Automatically discovers all Buildbot builds associated with a PR
- Checks the status of triggered sub-builds
- Maps build request IDs to system names (for NixOS builds)
- Provides raw log URLs for failed builds
- Exits with appropriate status codes for CI integration

## Installation

```bash
nix run .#buildbot-pr-check -- <pr-url>
```

## Usage

```bash
# GitHub PR
buildbot-pr-check https://github.com/TUM-DSE/doctor-cluster-config/pull/459

# Gitea PR
buildbot-pr-check https://git.clan.lol/clan/clan-core/pulls/4210
```

### GitHub Authentication

The tool automatically uses GitHub authentication in the following order:

1. `GITHUB_TOKEN` environment variable
2. Token from `gh` CLI (if installed and authenticated)

```bash
# Using environment variable
export GITHUB_TOKEN=your_github_token
buildbot-pr-check https://github.com/TUM-DSE/doctor-cluster-config/pull/459

# Or authenticate with gh CLI
gh auth login
buildbot-pr-check https://github.com/TUM-DSE/doctor-cluster-config/pull/459
```

## Demo Output

```
$ buildbot-pr-check https://github.com/TUM-DSE/doctor-cluster-config/pull/459
🔎 Checking PR #459 in TUM-DSE/doctor-cluster-config (github)
════════════════════════════════════════════════════════════════════════════════
Found 30 buildbot build(s)

Found 1 build(s) with triggered sub-builds

🔍 Checking: https://buildbot.dse.in.tum.de/#/builders/18/builds/712
────────────────────────────────────────────────────────────────────────────────
Found 35 triggered builds

📊 Build Summary:
  ❌ FAILURE: 4 builds
  ✅ SUCCESS: 31 builds

❌ Failed builds (4 total):
  → checks.x86_64-linux.nixos-martha
    Log URLs:
      • Build flake attr (stdio): https://buildbot.dse.in.tum.de/api/v2/logs/41100/raw_inline
  → checks.x86_64-linux.nixos-jack
    Log URLs:
      • Build flake attr (stdio): https://buildbot.dse.in.tum.de/api/v2/logs/41103/raw_inline
  → checks.x86_64-linux.nixos-ruby
    Log URLs:
      • Build flake attr (stdio): https://buildbot.dse.in.tum.de/api/v2/logs/41108/raw_inline
  → checks.x86_64-linux.nixos-tegan
    Log URLs:
      • Build flake attr (stdio): https://buildbot.dse.in.tum.de/api/v2/logs/41111/raw_inline
```

## Output Details

The script provides:

1. **Build Discovery**: Automatically finds all Buildbot builds from PR statuses
2. **Build Filtering**: Only shows builds that have triggered sub-builds
3. **Status Summary**: Color-coded summary with Unicode icons for quick scanning
4. **Failed Builds**: Displays flake attributes and direct links to build logs

## Exit Codes

- `0`: All builds passed successfully
- `1`: One or more builds were canceled or failed

## Testing

The project includes integration tests using VCR.py to record and replay HTTP
requests:

```bash
# Run tests with direnv
direnv exec . pytest tests/ -v
```

The tests use pre-recorded HTTP cassettes, so they don't require network access
or API credentials.
