{
  pkgs,
  writeShellApplication,
  claude-code,
  pexpect-mcp,
}:

writeShellApplication {
  name = "claude";
  runtimeInputs = [
    claude-code
    pexpect-mcp
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

    # if no arguments are provided:
    if [ "$#" -eq 0 ]; then
       claude mcp add pexpect -- pexpect-mcp || true
    fi

    # Run the actual claude command
    exec claude "$@"
  '';
}
