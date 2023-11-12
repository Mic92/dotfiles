{ config, ... }: {
  services.buildbot-nix.master = {
    enable = true;
    domain = "buildbot.thalheim.io";
    workersFile = config.sops.secrets.buildbot-nix-workers.path;
    buildSystems = [
      "aarch64-linux"
      "x86_64-linux"
      "aarch64-darwin"
    ];
    evalWorkerCount = 8;
    github = {
      tokenFile = config.sops.secrets.buildbot-github-token.path;
      webhookSecretFile = config.sops.secrets.buildbot-github-webhook-secret.path;
      oauthSecretFile = config.sops.secrets.buildbot-github-oauth-secret.path;
      oauthId = "d1b24258af1abc157934";
      user = "mic92-buildbot";
      admins = [ "Mic92" "DavHau" "Lassulus" ];
    };
    outputsPath = "/var/www/buildbot/nix-outputs";
  };
  services.buildbot-nix.worker = {
    enable = true;
    workerPasswordFile = config.sops.secrets.buildbot-nix-worker-password.path;
  };

  services.nginx.virtualHosts."buildbot.thalheim.io" = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
  };
}
