"""Generate herdr's plugins.json registry from Nix-built plugin directories.

herdr keeps its plugin registry in ~/.config/herdr/plugins.json and normally
writes it via `herdr plugin install/link`. We own that file declaratively:
each plugin is a store path containing herdr-plugin.toml, and herdr reloads
all manifest details from manifest_path on startup, so the registry only
needs the identifying fields plus enabled/source.
"""

import json
import sys
import tomllib
from pathlib import Path


def entry(root: str) -> dict[str, object]:
    manifest_path = Path(root) / "herdr-plugin.toml"
    with manifest_path.open("rb") as f:
        manifest = tomllib.load(f)
    return {
        "plugin_id": manifest["id"],
        "name": manifest["name"],
        "version": manifest["version"],
        "min_herdr_version": manifest.get("min_herdr_version", ""),
        "manifest_path": str(manifest_path),
        "plugin_root": root,
        "enabled": True,
        "source": {"kind": "local"},
    }


def main() -> None:
    entries = sorted(
        (entry(root) for root in sys.argv[1:]), key=lambda e: str(e["plugin_id"])
    )
    json.dump(entries, sys.stdout, indent=2)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
