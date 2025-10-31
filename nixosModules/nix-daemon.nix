{
  lib,
  config,
  pkgs,
  self,
  ...
}:
{
  # this extends srvos's common settings
  nix = {
    package = self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix;
    gc.automatic = true;
    gc.dates = "03:15";
    gc.options = "--delete-older-than 10d";

    # set legacy nixpkgs path to flake reference
    nixPath = [ "nixpkgs=flake:nixpkgs" ];

    settings = {
      # for nix-direnv
      keep-outputs = true;
      keep-derivations = true;

      # in zfs we trust
      fsync-metadata = lib.boolToString (
        !config.boot.isContainer or config.fileSystems."/".fsType != "zfs"
      );
      substituters = [
        "https://hetzner-cache.numtide.com"

        "https://nix-community.cachix.org"
      ]
      ++ lib.optional (config.networking.hostName != "eve") "https://cache.thalheim.io";
      trusted-substituters = [
        "https://nix-community.cachix.org"
      ]
      ++ lib.optional (config.networking.hostName != "eve") "https://cache.thalheim.io";
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.thalheim.io-1:R7msbosLEZKrxk/lKxf9BTjOOH7Ax3H0Qj0/6wiHOgc="
      ];

      trusted-users = [
        "@wheel"
        "root"
      ];

      fallback = true;
      warn-dirty = false;
      auto-optimise-store = true;
    };
  };

  imports = [ ./builder.nix ];

  systemd.timers.nix-cleanup-gcroots = {
    timerConfig = {
      OnCalendar = [ "weekly" ];
      Persistent = true;
    };
    wantedBy = [ "timers.target" ];
  };
  systemd.services.nix-cleanup-gcroots = {
    serviceConfig = {
      Type = "oneshot";
      ExecStart = [
        # delete automatic gcroots older than 30 days
        "${pkgs.findutils}/bin/find /nix/var/nix/gcroots/auto /nix/var/nix/gcroots/per-user -type l -mtime +30 -delete"
        # created by nix-collect-garbage, might be stale
        "${pkgs.findutils}/bin/find /nix/var/nix/temproots -type f -mtime +10 -delete"
        # delete broken symlinks
        "${pkgs.findutils}/bin/find /nix/var/nix/gcroots -xtype l -delete"
      ];
    };
  };

  programs.command-not-found.enable = false;
}
