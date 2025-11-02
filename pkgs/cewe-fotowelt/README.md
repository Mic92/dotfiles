# CEWE Fotowelt Package

This directory contains the Nix package definition for CEWE Fotowelt, a photo
book creation and printing software.

## Updating CEWE Fotowelt

To update CEWE Fotowelt to the latest version:

### 1. Find the Latest Version

Extract the latest version from the CEWE download API using the stable page URL:

```bash
PAGE_URL="https://www.cewe.de/bestellsoftware/danke.html?keyAccount=24441&product=391"
KEY_ACCOUNT=$(echo "$PAGE_URL" | sed -n 's/.*keyAccount=\([0-9]*\).*/\1/p')
AFFILIATE_ID="x_x_x_x_6822_x_06822-BsEZQQWOPUVGg"

curl -s "https://dls.photoprintit.com/api/getClient/${KEY_ACCOUNT}-de_DE/hps/${AFFILIATE_ID}/linux" | \
  gunzip | python3 -c "import re, sys; data = sys.stdin.read(); match = re.search(r\"HPS_VER\s*=\s*'([^']+)'\", data); print(match.group(1) if match else 'Not found')"
```

This will output the version in the format `X.Y.Z` (e.g., `8.0.5`).

### 2. Generate Updated Sources

Run the `generate-sources.py` script. If no version is specified, it will
automatically fetch the latest version:

```bash
cd /home/joerg/.homesick/repos/dotfiles/pkgs/cewe-fotowelt
python3 generate-sources.py
```

Or specify a version explicitly:

```bash
python3 generate-sources.py 8.0.5
```

This script will:

- Fetch the index file for the specified version from CEWE servers
- Parse the available packages (Linux x64 executables and libraries)
- Download and calculate SHA256 hashes for each package
- Generate an updated `sources.nix` file

### 3. Verify the Update

After generating the sources, verify that `sources.nix` has been updated
correctly:

```bash
cat sources.nix
```

Check that:

- The `version` field matches the version you specified
- All source URLs point to the correct version
- Hashes are present for all packages

### 4. Test the Build

Test that the package builds correctly:

```bash
nix build .#cewe-fotowelt
```

## Package Structure

- `default.nix`: Main package definition
- `sources.nix`: Generated file containing version and source URLs (DO NOT EDIT
  MANUALLY)
- `generate-sources.py`: Script to generate `sources.nix` for a given version
- `shell.nix`: Development shell for working with the package

## Notes

- The `sources.nix` file is automatically generated and should not be edited
  manually
- The package extracts multiple `.7z` archives containing the application
  binaries, libraries, and resources
- The build process uses `autoPatchelfHook` to fix library paths for the
  proprietary binaries
- Qt6 and OpenCV libraries require special symlink handling for proper
  versioning
