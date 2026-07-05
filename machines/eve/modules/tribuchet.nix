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
    # Prometheus metrics scraped locally by telegraf (see telegraf.nix).
    settings.metrics-listen = "127.0.0.1:7438";
    externalBuilders = {
      enable = true;
      # The hub derives external-builders and max-jobs from the workers
      # actually connected (eliza aarch64-linux, jamie x86_64-linux, and
      # whatever else joins), so we do not hard-code a systems list. A
      # declined build (no worker up) still falls back to a local build.
      dynamic = true;
      # nix-1 main already carries the uid-range patch (see flake input).
      patchNix = false;
      nixPackage = self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix;
    };
  };

  # An external build burns no local CPU (it relays to a worker), but it
  # still reserves a build slot AND an auto-allocated uid slot on eve
  # while it runs. max-jobs is raised well past eve's core count by the
  # hub (oversubscribed to keep every worker fed), so the uid pool must
  # not become the hidden ceiling: auto-allocate-uids hands out
  # id-count / 65536 slots, so size id-count to cover the hub's
  # max-jobs-cap (default 256 -> 256 * 65536).
  nix.settings.id-count = 256 * 65536;

  # nix-daemon.nix already sets nix.package; the externalBuilders option
  # sets it again, so force the single patched nix-1 definition.
  nix.package = lib.mkForce self.inputs.nix.packages.${pkgs.stdenv.hostPlatform.system}.nix;
}
