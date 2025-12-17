# Package Updater

Automatically update third-party packages in `pkgs/`.

## Usage

```bash
# Using the nix package (recommended, includes all dependencies)
nix run .#updater -- --list
nix run .#updater -- --dry-run
nix run .#updater -- --pr
nix run .#updater -- --pr -p <name>

# Or directly with Python (requires nix-update, gh in PATH)
cd pkgs
python3 -m updater --list       # List all updatable packages
python3 -m updater --dry-run    # Preview what would be updated
python3 -m updater              # Update all packages in place
python3 -m updater -p <name>    # Update a single package
python3 -m updater --pr         # Create a PR for each updated package
```

The `--pr` flag uses git worktrees to create PRs without affecting your current
checkout.

## Adding a New Package

### Option 1: nix-update (for standard GitHub/GitLab releases)

Create a `nix-update-args` file in your package directory:

```
pkgs/mypackage/
├── default.nix
└── nix-update-args    # Contains nix-update arguments
```

Example `nix-update-args`:

```
--version-regex v(.*)
```

### Option 2: Custom update.py (for non-standard sources)

Create an `update.py` script and `srcs.json` file:

```
pkgs/mypackage/
├── default.nix
├── srcs.json          # {"version": "...", "url": "...", "hash": "..."}
└── update.py          # Must define main() function
```

The `default.nix` should import `srcs.json`:

```nix
let
  srcs = lib.importJSON ./srcs.json;
in
stdenv.mkDerivation {
  pname = "mypackage";
  inherit (srcs) version;

  src = fetchurl {
    inherit (srcs) url hash;
  };
  # ...
}
```

Example `update.py`:

```python
#!/usr/bin/env python3
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))
from updater import update_srcs

def get_latest_version() -> dict:
    # Fetch latest version info from upstream
    return {"version": "1.0.0", "url": "https://..."}

def main() -> None:
    pkg_dir = Path(__file__).parent
    latest = get_latest_version()
    print(f"Latest version: {latest['version']}")
    update_srcs(pkg_dir, latest["version"], latest["url"])

if __name__ == "__main__":
    main()
```

## Shared Utilities

The `updater` module provides:

- `get_nix_hash(url)` - Fetch SRI hash using nix-prefetch-url
- `read_srcs(pkg_dir)` - Read srcs.json
- `write_srcs(pkg_dir, data)` - Write srcs.json
- `update_srcs(pkg_dir, version, url, hash=None)` - Update if version changed
  (fetches hash if not provided)
