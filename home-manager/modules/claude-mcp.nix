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
    }:
    pkgs.writeShellScriptBin package.pname ''
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "export ${name}=${value}") envVars)}
      exec ${package}/bin/${package.meta.mainProgram or package.pname} "$@"
    '';

  # Wrapped MCP servers with their required environment variables
  github-mcp-wrapped = wrapMcpServer {
    package = pkgs.github-mcp-server;
    envVars = {
      GITHUB_PERSONAL_ACCESS_TOKEN = "$(${pkgs.gh}/bin/gh auth token 2>/dev/null || { echo 'Warning: Failed to get GitHub token from gh auth' >&2; exit 1; })";
    };
  };

  gitea-mcp-wrapped = wrapMcpServer {
    package = self.packages.${pkgs.system}.gitea-mcp;
    envVars = {
      GITEA_TOKEN = "$(${pkgs.gawk}/bin/awk '/token:/ {print $2}' $HOME/.config/tea/config.yml 2>/dev/null || { echo 'Warning: Failed to get Gitea token from tea config' >&2; exit 1; })";
    };
  };

  # Create the claude-code package with the server packages
  claude-code-pkg = self.packages.${pkgs.system}.claude-code.override {
    servers = {
      github = github-mcp-wrapped;
      gitea = gitea-mcp-wrapped;
    };
  };
in
{
  home.packages = claude-code-pkg.packages;
}
