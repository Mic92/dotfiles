{ lib, config, pkgs, ... }: with lib; {
  nix = {
    trustedUsers = [ "joerg" "root" ];
    gc.automatic = true;
    gc.dates = "03:15";
    package = pkgs.nixFlakes.override {
      patches = [ ./unset-is-macho.patch ];
    };

    # should be enough?
    nrBuildUsers = lib.mkDefault 32;

    # https://github.com/NixOS/nix/issues/719
    extraOptions = ''
      builders-use-substitutes = true
      keep-outputs = true
      keep-derivations = true
      # in zfs we trust
      fsync-metadata = ${lib.boolToString (!config.boot.isContainer or config.fileSystems."/".fsType != "zfs")}
      experimental-features = nix-command flakes
    '';

    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://mic92.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "mic92.cachix.org-1:gi8IhgiT3CYZnJsaW7fxznzTkMUOn1RY4GmXdT/nXYQ="
    ];
  };

  imports = [ ./builder.nix ];

  programs.command-not-found.enable = false;

  nixpkgs.config.allowUnfree = true;
}
