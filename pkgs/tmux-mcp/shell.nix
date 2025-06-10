{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    python3
    python3Packages.pip
    python3Packages.pytest
    python3Packages.pytest-asyncio
    python3Packages.mcp
    python3Packages.pydantic
    python3Packages.anyio
  ];

  shellHook = ''
    export PYTHONPATH="$PWD:$PYTHONPATH"
    echo "tmux-mcp development shell"
    echo "Run: pytest tests/ -v"
  '';
}
