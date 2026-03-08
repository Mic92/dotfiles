{
  config,
  self,
  pkgs,
  ...
}:
{
  services.opencrow.skills.paperless = ./skills/paperless;

  clan.core.vars.generators.opencrow-paperless = {
    files.paperless-api-token.secret = true;

    prompts.paperless-api-token.description = "Paperless-ngx API token for opencrow";

    script = ''
      cp "$prompts/paperless-api-token" "$out/paperless-api-token"
    '';
  };

  services.opencrow.rbwEntries."paperless-api-token" = "paperless-api-token";

  services.opencrow.credentialFiles."paperless-api-token" =
    config.clan.core.vars.generators.opencrow-paperless.files.paperless-api-token.path;

  services.opencrow.extraPackages = [
    self.inputs.freelancer-toolbox.packages.${pkgs.stdenv.hostPlatform.system}.paperless-cli
  ];

  services.opencrow.environment = {
    PAPERLESS_URL = "https://paperless-api.thalheim.io";
  };

  containers.opencrow.config.systemd.tmpfiles.rules = [
    "d /var/lib/opencrow/.config/paperless-cli 0750 opencrow opencrow -"
    ''f /var/lib/opencrow/.config/paperless-cli/config.json 0640 opencrow opencrow - {"url":"https://paperless-api.thalheim.io","token_command":"rbw get paperless-api-token"}''
  ];
}
