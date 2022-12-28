{
  lib,
  config,
  inputs,
  ...
}:
with lib; {
  nix = {
    gc.automatic = true;
    gc.dates = "03:15";
    gc.options = "--delete-older-than 10d";
    # should be enough?
    nrBuildUsers = lib.mkDefault 32;

    daemonIOSchedClass = "idle";
    daemonCPUSchedPolicy = "idle";

    settings = {
      # https://github.com/NixOS/nix/issues/719
      builders-use-substitutes = true;

      # for nix-direnv
      keep-outputs = true;
      keep-derivations = true;

      # in zfs we trust
      fsync-metadata = lib.boolToString (!config.boot.isContainer or config.fileSystems."/".fsType != "zfs");
      experimental-features = "nix-command flakes";
      substituters = [
        "https://nix-community.cachix.org"
        #"https://mic92.cachix.org"
        "https://cache.thalheim.io"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        #"mic92.cachix.org-1:gi8IhgiT3CYZnJsaW7fxznzTkMUOn1RY4GmXdT/nXYQ="
        "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc="
      ];
      trusted-users = ["joerg" "root"];

      connect-timeout = 5;
      log-lines = 25;
      min-free = 128000000;
      max-free = 1000000000;

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
