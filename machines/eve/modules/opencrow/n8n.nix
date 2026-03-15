{
  config,
  pkgs,
  self,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;

  n8n-workflows-skill = pkgs.writeTextDir "SKILL.md" ''
    ---
    name: n8n-workflows
    description: Manage n8n workflow definitions. Use when asked to create, edit, inspect n8n workflows, or set up recurring/scheduled tasks and automations.
    ---

    For n8n-cli API reference, see [SKILL.md](${micsSkills}/skills/n8n-cli/SKILL.md).

    # n8n Workflows

    Workflow definitions live in `~/n8n-workflows`. Read its `CLAUDE.md` for
    detailed conventions, schema, and deployment instructions before making changes.

    ```bash
    cd ~/n8n-workflows
    cat CLAUDE.md
    ```
  '';
in
{
  services.opencrow.skills.n8n-workflows = n8n-workflows-skill;

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

  containers.opencrow.config.systemd.tmpfiles.rules = [
    "d /var/lib/opencrow/.config/n8n-cli 0750 opencrow opencrow -"
    ''f /var/lib/opencrow/.config/n8n-cli/config.json 0640 opencrow opencrow - {"api_url":"https://n8n-api.thalheim.io","api_key_command":"rbw get n8n-api-jwt"}''
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
