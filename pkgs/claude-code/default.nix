{
  pkgs,
  writeShellApplication,
  claude-code,
  servers ? { },
  keepServers ? { },
}:

let
  claude-wrapper = writeShellApplication {
    name = "claude";
    runtimeInputs = [ claude-code pkgs.pueue ];
    text = ''
      set -euo pipefail

      # Start pueued daemon if not already running
      if ! pueue status &>/dev/null; then
        echo "Starting pueue daemon..."
        pueued -d
        # Give it a moment to start
        sleep 0.5
      fi

      # Define MCP plugins as associative array (name -> command)
      # Use nix-profile paths instead of direct package paths
      declare -A mcp_plugins=(
       ${pkgs.lib.concatStringsSep "\n" (
         pkgs.lib.mapAttrsToList (
           name: package: "[\"${name}\"]=\"$HOME/.nix-profile/bin/${package.name}\""
         ) servers
       )}
      )
      declare -A ignore_plugins=(
        ${pkgs.lib.concatStringsSep "\n" (
          pkgs.lib.mapAttrsToList (name: _: "[\"${name}\"]=1") keepServers
        )}
      )

      # Get existing MCP servers as associative array
      declare -A existing_servers=()
      while IFS= read -r server; do
        [ -n "$server" ] && existing_servers["$server"]=1
      done < <(claude mcp list 2>/dev/null | awk -F': ' 'NF {print $1}' || true)

      # Remove unwanted servers (except whitelisted ones)
      for server in "''${!existing_servers[@]}"; do
        if [[ ! "''${mcp_plugins[$server]+exists}" ]] && [[ ! "''${ignore_plugins[$server]+exists}" ]]; then
          echo "Removing unwanted MCP server: $server"
          claude mcp remove "$server" 2>/dev/null || true
        fi
      done

      # Add missing servers
      for plugin in "''${!mcp_plugins[@]}"; do
        if [[ ! "''${existing_servers[$plugin]+exists}" ]]; then
          echo "Setting up $plugin MCP server..."
          claude mcp add "$plugin" -- "''${mcp_plugins[$plugin]}"
        fi
      done

      # Run the actual claude command (wrapped MCP servers handle their own credentials)
      exec claude "$@"
    '';
  };
in
claude-wrapper.overrideAttrs (oldAttrs: {
  passthru = (oldAttrs.passthru or { }) // {
    inherit servers;
    packages = [ claude-wrapper ] ++ (pkgs.lib.attrValues servers);
  };
})
