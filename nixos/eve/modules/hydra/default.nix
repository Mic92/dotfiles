{ config, lib, pkgs, ... }:

{
  nix.allowedUsers = [ "hydra" "hydra-queue-runner" "hydra-www" ];

  systemd.tmpfiles.rules = [
    "d /var/lib/hydra/build-logs 0775 hydra-queue-runner hydra"
    "d /var/lib/hydra/scm/ 0755 hydra hydra"
  ];

  services.nginx.virtualHosts."hydra.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:3003;
    '';
  };

  imports = [
    ./declarative-admin-user.nix
  ];

  sops.secrets.hydra-admin-password.owner = "hydra-www";

  services.hydra = {
    enable = true;
    hydraURL = "https://hydra.thalheim.io";
    useSubstitutes = true;
    minimumDiskFree = 100;
    minimumDiskFreeEvaluator = 1;
    admin.passwordFile = config.sops.secrets.hydra-admin-password.path;
    extraConfig = ''
      evaluator_max_memory_size = 4096
      evaluator_initial_heap_size = ${toString (4 * 1024 * 1024 * 1024)}
      max_concurrent_evals = 1
      evaluator_workers = 4
    '';

    notificationSender = "dontspamme@example.com";

    port = 3003;

    package = pkgs.hydra-unstable.overrideAttrs (old: with pkgs; let
      in {
        patches = let
          prPatches = {
            # system load
            "689" = "sha256:1q88z3a0hwrylhjc8qhrklggk92hzi1appha3kjrlvz2k2rvjjga";
            # restart buttons
            "836" = "sha256-v0DykNZNGsq+cMxY0NIuilJgLIcQyzSCq6V7Qs2kAk0=";
            # GitInput: Avoid fetch if rev is available
            "868" = "sha256-dTFGWhfiANs5Z5ScmZxFaCZPZfAVBJOceEOvVMHShSw=";
            # queue-runner status in topbar
            "869" = "sha256-iFpMV4Z5VE7YmvnAHAydUQYk0FNKNarPYqRHEAuT4/w=";
            # tailon
            "957" = "sha256:0lbrs04z5vdkj8xr7pa4yfydpi295300zrza9zrjbapwp2q1jr1y";
            # improved UI
            "965" = "sha256-VJ6JwPFWwT5qy+dBsqecnvKtmHZ+VemCqYE6EmiWQQM=";
          };
        in old.patches ++
           lib.mapAttrsToList (n: sha256: pkgs.fetchpatch {
             url = "https://patch-diff.githubusercontent.com/raw/NixOS/hydra/pull/${n}.patch";
             inherit sha256;
           }) prPatches;
        buildInputs = old.buildInputs ++ (with perlPackages; [
          LinuxInotify2
          CatalystPluginPrometheusTiny
        ]);

        prePatch = ''
          sed -i 's/evalSettings.restrictEval = true/evalSettings.restrictEval = false/' "$(find -name hydra-eval-jobs.cc)"
        '';
        doCheck = false;
      }
    );
  };
}
