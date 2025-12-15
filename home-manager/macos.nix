{
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./modules/calendar.nix
    ./modules/ai.nix
    ./modules/mail.nix
    ./modules/atuin-autosync.nix
    ./modules/librewolf.nix
  ];
  home.packages = [
    pkgs.eternal-terminal
    pkgs.rbw
    inputs.strace-macos.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.niks3.packages.${pkgs.stdenv.hostPlatform.system}.niks3
  ];
}
