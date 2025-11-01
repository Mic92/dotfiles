{ pkgs, self, ... }:
{
  # this extends srvos's common settings
  nix = {
    gc.automatic = true;
    gc.interval = {
      Hour = 3;
      Minute = 15;
    };
    gc.options = "--delete-older-than 10d";
    package = self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix;
    #package = pkgs.nixVersions.latest;

    settings = {
      # for nix-direnv
      keep-outputs = true;
      keep-derivations = true;

      substituters = [
        "https://hetzner-cache.numtide.com"

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

  launchd.daemons.nix-daemon = {
    serviceConfig.Nice = -10;
  };
}
