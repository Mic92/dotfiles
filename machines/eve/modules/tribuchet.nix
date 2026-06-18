{
  config,
  pkgs,
  self,
  lib,
  ...
}:
let
  inherit (self.inputs) tribuchet;
  tribuchetPkg = tribuchet.packages.${pkgs.stdenv.hostPlatform.system}.default;
  gen = config.clan.core.vars.generators.tribuchet.files;
in
{
  imports = [ tribuchet.nixosModules.default ];

  # CA, hub key pair and the worker key pair share one generator so they
  # come from a single self-signed root. The worker key pair (worker.crt,
  # worker.key) is consumed by the eliza worker in doctor-cluster-config.
  clan.core.vars.generators.tribuchet = {
    files = {
      "ca.crt".secret = false;
      "ca.key" = { };
      "hub.crt".secret = false;
      "hub.key" = { };
      "worker.crt".secret = false;
      "worker.key" = { };
    };
    runtimeInputs = [ tribuchetPkg ];
    # The hub certificate SAN must match the address workers dial; eve is
    # reachable on the clear net at eve.thalheim.io.
    script = ''
      tribuchet ca init --dir "$out"
      tribuchet ca issue eve.thalheim.io --dir "$out"
      tribuchet ca issue worker --dir "$out"
      mv "$out/eve.thalheim.io.crt" "$out/hub.crt"
      mv "$out/eve.thalheim.io.key" "$out/hub.key"
    '';
  };

  # The hub reads <configDir>/ca/{hub.crt,hub.key,ca.crt}.
  environment.etc."tribuchet/ca/ca.crt".source = gen."ca.crt".path;
  environment.etc."tribuchet/ca/hub.crt".source = gen."hub.crt".path;
  environment.etc."tribuchet/ca/hub.key".source = gen."hub.key".path;

  services.tribuchet-hub = {
    enable = true;
    openFirewall = true;
    externalBuilders = {
      enable = true;
      # eve is x86_64; hand aarch64-linux builds to the eliza worker.
      systems = [ "aarch64-linux" ];
      # nix-1 main already carries the uid-range patch (see flake input).
      patchNix = false;
      nixPackage = self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix;
    };
  };

  # nix-daemon.nix already sets nix.package; the externalBuilders option
  # sets it again, so force the single patched nix-1 definition.
  nix.package = lib.mkForce self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix;
}
