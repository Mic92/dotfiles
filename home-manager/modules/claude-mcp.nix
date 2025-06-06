{
  inputs,
  pkgs,
  self,
  ...
}:

let
  # Claude wrapper script that ensures MCP server is configured and runs claude
  claudeWrapper = pkgs.writeShellScriptBin "claude" ''
    set -euo pipefail

    # Get the current MCP server list once
    mcp_list=$(${pkgs.claude-code}/bin/claude mcp list 2>/dev/null || echo "")

    # Define MCP plugins as associative array (name -> command)
    declare -A mcp_plugins=(
      ["github"]="${
        inputs.mcp-servers-nix.packages.${pkgs.system}.mcp-server-github
      }/bin/mcp-server-github"
      ["gitea"]="${self.packages.${pkgs.system}.gitea-mcp}/bin/gitea-mcp"
    )

    # Define environment variables for plugins
    declare -A mcp_envs=(
      ["github"]="GITHUB_PERSONAL_ACCESS_TOKEN=$(${pkgs.gh}/bin/gh auth token)"
      ["gitea"]=""
    )

    # Setup missing MCP plugins
    for plugin in "''${!mcp_plugins[@]}"; do
      if ! echo "$mcp_list" | grep -q "$plugin"; then
        echo "Setting up $plugin MCP server..."
        if [ -n "''${mcp_envs[$plugin]}" ]; then
          ${pkgs.claude-code}/bin/claude mcp add "$plugin" -e "''${mcp_envs[$plugin]}" -- "''${mcp_plugins[$plugin]}"
        else
          ${pkgs.claude-code}/bin/claude mcp add "$plugin" -- "''${mcp_plugins[$plugin]}"
        fi
      fi
    done

    # Run the actual claude command
    exec ${pkgs.claude-code}/bin/claude "$@"
  '';

in
{
  home.packages = [
    inputs.mcp-servers-nix.packages.${pkgs.system}.mcp-server-github
    self.packages.${pkgs.system}.gitea-mcp
    claudeWrapper
  ];
}
