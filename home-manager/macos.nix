{ pkgs, ... }:
{
  imports = [ ./modules/claude-mcp.nix ];
  home.packages = [
    pkgs.eternal-terminal
    pkgs.rbw
  ];
}
