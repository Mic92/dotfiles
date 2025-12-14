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

  # Cleanup old gcroots and broken symlinks weekly (similar to NixOS systemd service)
  launchd.daemons.nix-cleanup-gcroots = {
    script = ''
      set -eu
      # delete automatic gcroots older than 30 days
      ${pkgs.findutils}/bin/find /nix/var/nix/gcroots/auto /nix/var/nix/gcroots/per-user -type l -mtime +30 -delete || true
      # created by nix-collect-garbage, might be stale
      ${pkgs.findutils}/bin/find /nix/var/nix/temproots -type f -mtime +10 -delete || true
      # delete broken symlinks
      ${pkgs.findutils}/bin/find /nix/var/nix/gcroots -xtype l -delete || true
    '';
    serviceConfig = {
      StartCalendarInterval = [
        {
          Weekday = 0; # Sunday
          Hour = 3;
          Minute = 30;
        }
      ];
    };
  };
}
