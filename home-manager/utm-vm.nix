{ ... }:
{
  imports = [
    ./common.nix
    ./modules/ai.nix
    ./modules/atuin-autosync.nix
    ./modules/mail.nix
  ];
}
