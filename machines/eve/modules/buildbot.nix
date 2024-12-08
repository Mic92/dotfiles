{ config, pkgs, ... }:
{
  services.buildbot-nix.master = {
    enable = true;
    domain = "buildbot.thalheim.io";

    jobReportLimit = 1;

    workersFile = config.sops.secrets.buildbot-nix-workers.path;
    buildSystems = [
      "i686-linux"
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
    ];
    pullBased.repositories.clan = {
      url = "https://git.clan.lol/clan/clan-core.git";
      defaultBranch = "main";
    };
    branches.releaseBranches.matchGlob = "test-*";
    evalWorkerCount = 6;
    github = {
      webhookSecretFile = config.sops.secrets.buildbot-github-webhook-secret.path;

      oauthId = "Iv23ctDGhrm116Be1LhO";
      oauthSecretFile = config.sops.secrets.buildbot-github-oauth-secret.path;

      authType.app = {
        id = 915265;
        secretKeyFile = config.sops.secrets.buildbot-github-app-secret-key.path;
      };
    };
    admins = [
      "Mic92"
      "DavHau"
      "Lassulus"
    ];
    outputsPath = "/var/www/buildbot/nix-outputs/";
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
  };

  services.nginx.virtualHosts."buildbot.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
  };
}
