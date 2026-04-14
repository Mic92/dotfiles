{
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./modules/alertmanager-bar.nix
    ./modules/nostr-chat.nix
    ./modules/calendar.nix
    ./modules/ai.nix
    ./modules/kimai.nix
    ./modules/mail.nix
    ./modules/atuin-autosync.nix
  ];
  home.packages = [
    pkgs.eternal-terminal
    pkgs.rbw
    pkgs.radicle-node
    inputs.strace-macos.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.niks3.packages.${pkgs.stdenv.hostPlatform.system}.niks3
  ];
}
