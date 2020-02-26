{...}: {
  nix = {
    trustedUsers = ["joerg" "root"];
    useSandbox = true;
    buildCores = 0;
    gc.automatic = true;
    gc.dates = "03:15";

    # should be enough?
    nrBuildUsers = 32;

    # https://github.com/NixOS/nix/issues/719
    extraOptions = ''
      builders-use-substitutes = true
      max-jobs = auto
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';

    nixPath = [
      # provided by krops
      "nixpkgs=/var/src/nixpkgs"
      "home-manager=/var/src/home-manager"
      "secrets=/var/src/secrets"
      "nur=/var/src/nur"
      "shared-secrets=/var/src/shared-secrets"
      "nixos-config=/var/src/nixos-config"
      "nixos-hardware=/var/src/nixos-hardware"

      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  imports = [ ./builder.nix ];

  nixpkgs.config.allowUnfree = true;
}
