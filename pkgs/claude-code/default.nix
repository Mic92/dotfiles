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

    # Start pueued daemon if not already running
    if ! pueue status &>/dev/null; then
      echo "Starting pueue daemon..."
      pueued -d
      # Give it a moment to start
      sleep 0.5
    fi

    # Run the actual claude command
    exec claude "$@"
  '';
}
