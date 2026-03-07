{
  config,
  self,
  pkgs,
  ...
}:
{
  clan.core.vars.generators.opencrow-n8n = {
    files.n8n-api-jwt.secret = true;

    prompts.n8n-api-jwt.description = "n8n API JWT token for workflow management";

    script = ''
      cp "$prompts/n8n-api-jwt" "$out/n8n-api-jwt"
    '';
  };

  services.opencrow.credentialFiles = {
    "n8n-api-jwt" = config.clan.core.vars.generators.opencrow-n8n.files.n8n-api-jwt.path;
  };

  services.opencrow.extraPackages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.n8n-cli
  ];

  # Clone the n8n-workflows repo into the agent's workspace.
  containers.opencrow.config.systemd.services.opencrow-clone-repos = {
    description = "Clone git repos into opencrow workspace";
    wantedBy = [ "multi-user.target" ];
    after = [ "opencrow-ssh-keys.service" ];
    requires = [ "opencrow-ssh-keys.service" ];
    before = [ "opencrow.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = "opencrow";
      Group = "opencrow";
      WorkingDirectory = "/var/lib/opencrow";
    };
    path = [
      pkgs.git
      pkgs.openssh
    ];
    script = ''
      export GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=accept-new"
      if [ ! -d /var/lib/opencrow/n8n-workflows/.git ]; then
        git clone gitea@git.thalheim.io:Mic92/n8n-workflows.git /var/lib/opencrow/n8n-workflows
      fi
    '';
  };
}
