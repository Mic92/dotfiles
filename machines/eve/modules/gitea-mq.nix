# gitea-mq merge queue, GitHub backend only.
#
# Dashboard / webhook: https://mq.thalheim.io
# GitHub App webhook:  https://mq.thalheim.io/webhook/github
#
# The GitHub App is created via https://mic92.github.io/gitea-mq/ and
# installed on the orgs/repos that should be queued. App ID lives in this
# file, the private key and webhook secret come from clan vars.
{
  config,
  pkgs,
  self,
  ...
}:
let
  domain = "mq.thalheim.io";
  port = 18980;
  gen = config.clan.core.vars.generators;
in
{
  imports = [ self.inputs.gitea-mq.nixosModules.default ];

  services.gitea-mq = {
    enable = true;
    listenAddr = "127.0.0.1:${toString port}";
    externalUrl = "https://${domain}";

    # GitHub-only instance; the local Gitea is not wired up.
    hideRefFromClients = false;

    github = {
      appId = 3518559;
      privateKeyFile = gen.gitea-mq-github-key.files.private-key.path;
      webhookSecretFile = gen.gitea-mq-github.files.webhook-secret.path;
    };
  };

  # DynamicUser resolves to the unit name, so the peer-auth role must match.
  services.postgresql.ensureDatabases = [ "gitea-mq" ];
  services.postgresql.ensureUsers = [
    {
      name = "gitea-mq";
      ensureDBOwnership = true;
    }
  ];

  services.nginx.virtualHosts.${domain} = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://127.0.0.1:${toString port};
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
    '';
  };

  # gitea-mq pushes the webhook secret to the App on startup, so it can be
  # generated here and never touched in the GitHub UI.
  clan.core.vars.generators.gitea-mq-github = {
    files.webhook-secret = { };
    runtimeInputs = [ pkgs.xkcdpass ];
    script = ''
      xkcdpass -n 6 -d - > "$out/webhook-secret"
    '';
  };

  # Populated via `clan vars set` from the .pem GitHub generates.
  clan.core.vars.generators.gitea-mq-github-key = {
    files.private-key = { };
    prompts.private-key = {
      description = "GitHub App private key (PEM) for gitea-mq";
      type = "multiline";
      persist = true;
    };
    script = ''
      cp "$prompts/private-key" "$out/private-key"
    '';
  };
}
