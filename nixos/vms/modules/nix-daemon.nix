{...}: {
  nix = {
    trustedUsers = ["joerg"];
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
  };

  imports = [ ./builder.nix ];

  nixpkgs.config.allowUnfree = true;
}
