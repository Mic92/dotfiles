{
  config,
  self,
  pkgs,
  ...
}:
{
  services.opencrow.skills.n8n-workflows = ./skills/n8n-workflows;

  clan.core.vars.generators.opencrow-n8n = {
    files.n8n-api-jwt.secret = true;

    prompts.n8n-api-jwt.description = "n8n API JWT token for workflow management";

    script = ''
      cp "$prompts/n8n-api-jwt" "$out/n8n-api-jwt"
    '';
  };

  services.opencrow.rbwEntries."n8n-api-jwt" = "n8n-api-jwt";

  services.opencrow.credentialFiles."n8n-api-jwt" =
    config.clan.core.vars.generators.opencrow-n8n.files.n8n-api-jwt.path;

  services.opencrow.extraPackages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.n8n-cli
  ];

  # Clone the n8n-workflows repo on first start (runs after SSH keys
  # are installed by gitea.nix's ExecStartPre).
  containers.opencrow.config.systemd.services.opencrow.serviceConfig.ExecStartPre = [
    "${pkgs.writeShellScript "clone-n8n-workflows" ''
      export PATH="${pkgs.git}/bin:${pkgs.openssh}/bin:$PATH"
      export GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=accept-new"
      if [ ! -d /var/lib/opencrow/n8n-workflows/.git ]; then
        git clone gitea@git.thalheim.io:Mic92/n8n-workflows.git /var/lib/opencrow/n8n-workflows
      fi
    ''}"
  ];
}
