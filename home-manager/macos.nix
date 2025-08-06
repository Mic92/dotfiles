{ pkgs, ... }:
{
  imports = [
    ./modules/calendar.nix
    ./modules/ai.nix
    ./modules/mail.nix
  ];
  home.packages = [
    pkgs.eternal-terminal
    pkgs.rbw
  ];
}
