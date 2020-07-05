{ lib, config, ... }: with lib; {
  nix = {
    trustedUsers = ["joerg" "root"];
    useSandbox = true;
    gc.automatic = true;
    gc.dates = "03:15";

    # should be enough?
    nrBuildUsers = 32;

    # https://github.com/NixOS/nix/issues/719
    extraOptions = ''
      builders-use-substitutes = true
      keep-outputs = true
      keep-derivations = true
      # in zfs we trust
      fsync-metadata = ${lib.boolToString (config.fileSystems."/".fsType != "zfs")}
      experimental-features = nix-command flakes
    '';

    nixPath = let
      kropsSources = filter (src: src != ".populate")
        (attrNames (builtins.readDir "/var/src"));
    in (map (entry: "${entry}=/var/src/${entry}") kropsSources) ++ [
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    binaryCaches = [
      "https://r-ryantm.cachix.org"
      "https://mic92.cachix.org"
    ];
    binaryCachePublicKeys = [
      "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
      "mic92.cachix.org-1:gi8IhgiT3CYZnJsaW7fxznzTkMUOn1RY4GmXdT/nXYQ="
    ];
  };

  imports = [ ./builder.nix ];

  nixpkgs.config.allowUnfree = true;
}
