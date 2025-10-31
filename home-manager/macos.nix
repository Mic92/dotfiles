{ pkgs, ... }:
{
  imports = [
    ./modules/calendar.nix
    ./modules/ai.nix
    ./modules/mail.nix
    ./modules/atuin-autosync.nix
  ];
  home.packages = [
    pkgs.eternal-terminal
    pkgs.rbw
  ];
}
