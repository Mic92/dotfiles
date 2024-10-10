{ pkgs, inputs, ... }:
{
  # this extends srvos's common settings
  nix = {
    gc.automatic = true;
    gc.interval = {
      Hour = 3;
      Minute = 15;
    };
    gc.options = "--delete-older-than 10d";
    package = inputs.nix.packages.${pkgs.hostPlatform.system}.nix;

    settings = {
      # for nix-direnv
      keep-outputs = true;
      keep-derivations = true;

      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.thalheim.io"
      ];
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://cache.thalheim.io"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc="
      ];

      trusted-users = [
        "joerg"
        "root"
      ];

      fallback = true;
      warn-dirty = false;
    };
  };
}
