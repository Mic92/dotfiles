{ config, ... }: {
  services.buildbot-nix.master = {
    enable = true;
    domain = "buildbot.thalheim.io";
    workersFile = config.sops.secrets.buildbot-nix-workers.path;
    github = {
      tokenFile = config.sops.secrets.buildbot-github-token.path;
      webhookSecretFile = config.sops.secrets.buildbot-github-webhook-secret.path;
      oauthSecretFile = config.sops.secrets.buildbot-github-oauth-secret.path;
      oauthId = "d1b24258af1abc157934";
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
