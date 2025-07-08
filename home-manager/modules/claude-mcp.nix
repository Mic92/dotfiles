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

  # Create the claude-code package with the server packages
  claude-code-pkg = self.packages.${pkgs.system}.claude-code.override {
    servers = {
      # No MCP servers configured by default
    };
    # added manually in some projects
    keepServers = {
      browser-mcp = true;
    };
  };
in
{
  home.packages = claude-code-pkg.packages ++ [
    self.packages.${pkgs.system}.claude-md
    pkgs.pueue
  ];
}
