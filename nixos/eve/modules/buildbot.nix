{ config, ... }: {
  services.buildbot-nix.master = {
    enable = true;
    domain = "buildbot.thalheim.io";
    workersFile = config.sops.secrets.buildbot-nix-workers.path;
    github = {
      tokenFile = config.sops.secrets.buildbot-github-token.path;
      webhookSecretFile = config.sops.secrets.buildbot-github-webhook-secret.path;
      oauthSecretFile = config.sops.secrets.buildbot-github-oauth-secret.path;
      oauthId = "2516248ec6289e4d9818122cce0cbde39e4b788d";
      githubUser = "mic92-buildbot";
      githubAdmins = [ "Mic92" ];
    };
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
