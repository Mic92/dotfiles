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
    export SHELL=bash

    # Start pueued daemon if not already running
    if ! pueue status &>/dev/null; then
      echo "Starting pueue daemon..."
      pueued -d
    fi

    # if no arguments are provided:
    if [ "$#" -eq 0 ]; then
       claude mcp add pexpect -- pexpect-mcp || true
       claude mcp add --transport http context7 https://mcp.context7.com/mcp --header "CONTEXT7_API_KEY: $(rbw get context7-api-key)" || true
    fi

    # Run the actual claude command
    exec claude "$@"
  '';
}
