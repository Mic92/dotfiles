{
  pkgs,
  self,
  lib,
  ...
}:

let
  # Helper function to create wrapped MCP servers with environment variables
  wrapMcpServer =
    {
      package,
      envVars ? { },
      args ? [ ],
    }:
    pkgs.writeShellScriptBin package.pname ''
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "export ${name}=${value}") envVars)}
      exec ${package}/bin/${
        package.meta.mainProgram or package.pname
      } ${lib.concatStringsSep " " args} "$@"
    '';

  gitea-mcp-wrapped = wrapMcpServer {
    package = self.packages.${pkgs.system}.gitea-mcp;
    envVars = {
      GITEA_TOKEN = "$(${pkgs.gawk}/bin/awk '/token:/ {print $2}' $HOME/.config/tea/config.yml 2>/dev/null || { echo 'Warning: Failed to get Gitea token from tea config' >&2; exit 1; })";
    };
  };

  tmux-mcp-wrapped = wrapMcpServer {
    package = self.packages.${pkgs.system}.tmux-mcp;
    envVars = { };
  };

  # Create the claude-code package with the server packages
  claude-code-pkg = self.packages.${pkgs.system}.claude-code.override {
    servers = {
      gitea = gitea-mcp-wrapped;
      tmux = tmux-mcp-wrapped;
    };
  };
in
{
  home.packages = claude-code-pkg.packages;
}
