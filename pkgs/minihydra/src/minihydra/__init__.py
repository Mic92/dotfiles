"""minihydra: per-commit nixpkgs eval/build differ.

The top-level package intentionally avoids importing :mod:`minihydra.cli` so
that scripts can `from minihydra import db` without pulling httpx, rich, etc.
The `minihydra` console script is wired to ``minihydra.cli:main`` directly
via ``[project.scripts]``.
"""

__all__: list[str] = []
