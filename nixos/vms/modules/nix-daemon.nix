{...}: {
  nix = {
    trustedUsers = ["joerg"];
    useSandbox = true;
    maxJobs = "auto";
    gc.automatic = true;
    gc.dates = "03:15";

    # should be enough?
    nrBuildUsers = 32;

    # https://github.com/NixOS/nix/issues/719
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';
  };
}
