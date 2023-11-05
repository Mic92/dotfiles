{ lib
, config
, pkgs
, ...
}:
{
  # this extends srvos's common settings
  nix = {
    gc.automatic = true;
    gc.dates = "03:15";
    gc.options = "--delete-older-than 10d";
    package = pkgs.nixVersions.unstable;

    settings = {
      # for nix-direnv
      keep-outputs = true;
      keep-derivations = true;

      # in zfs we trust
      fsync-metadata = lib.boolToString (!config.boot.isContainer or config.fileSystems."/".fsType != "zfs");
      substituters = [
        "https://nix-community.cachix.org"
        #"https://mic92.cachix.org"
        "https://cache.thalheim.io"
      ];
      trusted-substituters = [
        "https://nix-community.cachix.org"
        #"https://mic92.cachix.org"
        "https://cache.thalheim.io"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        #"mic92.cachix.org-1:gi8IhgiT3CYZnJsaW7fxznzTkMUOn1RY4GmXdT/nXYQ="
        "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc="
      ];

      trusted-users = [ "@wheel" "root" ];

      fallback = true;
      warn-dirty = false;
      auto-optimise-store = true;
    };
  };

  imports = [
    ./builder.nix
  ];

  programs.command-not-found.enable = false;
}
