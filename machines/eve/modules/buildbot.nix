{
  config,
  pkgs,
  ...
}:
{
  services.buildbot-nix.master = {
    enable = true;
    domain = "buildbot.thalheim.io";

    workersFile = config.sops.secrets.buildbot-nix-workers.path;
    buildSystems = [
      "i686-linux"
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ];
    branches.releaseBranches.matchGlob = "test-*";
    evalWorkerCount = 6;
    github = {
      webhookSecretFile = config.sops.secrets.buildbot-github-webhook-secret.path;

      oauthId = "Iv23ctDGhrm116Be1LhO";
      oauthSecretFile = config.sops.secrets.buildbot-github-oauth-secret.path;

      appId = 915265;
      appSecretKeyFile = config.sops.secrets.buildbot-github-app-secret-key.path;
    };
    admins = [
      "Mic92"
      "DavHau"
      "Lassulus"
    ];
    outputsPath = "/var/www/buildbot/nix-outputs/";
    pullBased = {
      repositories = {
        sizelint = {
          url = "https://github.com/a-kenji/sizelint";
          defaultBranch = "main";
        };
      };
    };
  };
  services.buildbot-master = {
    extraConfig = ''
      from buildbot.manhole import AuthorizedKeysManhole
      c['manhole'] = AuthorizedKeysManhole("tcp:12456", "/etc/ssh/authorized_keys.d/joerg", "/var/lib/buildbot/master/ssh/")
    '';
    pythonPackages = ps: [
      ps.bcrypt
      ps.cryptography
    ];
  };
  systemd.services.buildbot-master.path = [ pkgs.openssh ];
  systemd.services.buildbot-master.preStart = ''
    mkdir -p /var/lib/buildbot/master/ssh
    if [ ! -f /var/lib/buildbot/master/ssh/ssh_host_ed25519_key ]; then
      ssh-keygen -t ed25519 -f /var/lib/buildbot/master/ssh/ssh_host_ed25519_key -N ""
    fi
  '';
  services.buildbot-nix.worker = {
    enable = true;
    workerPasswordFile = config.sops.secrets.buildbot-nix-worker-password.path;
    # Broken atm
    #nixEvalJobs.package =
    #  (pkgs.nix-eval-jobs.override {
    #    nix = config.nix.package;
    #  }).overrideAttrs
    #    (_oldAttrs: {
    #      src = pkgs.fetchFromGitHub {
    #        owner = "nix-community";
    #        repo = "nix-eval-jobs";
    #        # https://github.com/nix-community/nix-eval-jobs/pull/341
    #        rev = "331aa136a3414f41e7524c9614d29a35122d6275";
    #        sha256 = "sha256-+ga7K6xenkpyoJgsM7ZYjacTLIoaCVxt33SzfTQrZpE=";
    #      };
    #    });

  };

  services.nginx.virtualHosts."buildbot.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
  };
}
