{ pkgs, ... }:
{
  imports = [
    ./modules/calendar.nix
    ./modules/claude-mcp.nix
    ./modules/mail.nix
  ];
  home.packages = [
    pkgs.eternal-terminal
    pkgs.rbw
  ];
}
