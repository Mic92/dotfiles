{
  pkgs,
  writeShellApplication,
  claude-code,
}:

writeShellApplication {
  name = "claude";
  runtimeInputs = [
    claude-code
    pkgs.pueue
  ];
  text = ''
    set -euo pipefail

    # Set shell to bash for Claude Code
    export SHELL=${pkgs.bashInteractive}/bin/bash

    # Start pueued daemon if not already running
    if ! pueue status &>/dev/null; then
      echo "Starting pueue daemon..."
      pueued -d
    fi

    # Run the actual claude command
    exec claude "$@"
  '';
}
