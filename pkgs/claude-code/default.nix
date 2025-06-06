{
  pkgs,
  writeShellScriptBin,
  claude-code,
  servers ? { },
}:

let
  claude-wrapper = writeShellScriptBin "claude" ''
    set -euo pipefail

    # Get the current MCP server list once
    mcp_list=$(${claude-code}/bin/claude mcp list 2>/dev/null || echo "")

    # Define MCP plugins as associative array (name -> command)
    # Using wrapped versions that handle credentials automatically
    declare -A mcp_plugins=(
     ${pkgs.lib.concatStringsSep "\n" (
       pkgs.lib.mapAttrsToList (name: package: "[\"${name}\"]=\"${pkgs.lib.getExe package}\"") servers
     )}
    )

    # Setup missing MCP plugins (without environment variables in config)
    for plugin in "''${!mcp_plugins[@]}"; do
      if ! echo "$mcp_list" | grep -q "$plugin"; then
        echo "Setting up $plugin MCP server..."
        ${claude-code}/bin/claude mcp add "$plugin" -- "''${mcp_plugins[$plugin]}"
      fi
    done

    # Run the actual claude command (wrapped MCP servers handle their own credentials)
    exec ${claude-code}/bin/claude "$@"
  '';
in
claude-wrapper.overrideAttrs (oldAttrs: {
  passthru = (oldAttrs.passthru or { }) // {
    inherit servers;
    packages = [ claude-wrapper ] ++ (pkgs.lib.attrValues servers);
  };
})
