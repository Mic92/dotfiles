{
  config,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  clan.core.vars.generators.opencrow-gmaps = {
    files.gmaps-api-key.secret = true;

    prompts.gmaps-api-key.description = "Google Maps API key for gmaps-cli";

    script = ''
      cp "$prompts/gmaps-api-key" "$out/gmaps-api-key"
    '';
  };

  services.opencrow.rbwEntries = {
    "google-maps-api-key" = "gmaps-api-key";
  };

  services.opencrow.credentialFiles = {
    "gmaps-api-key" = config.clan.core.vars.generators.opencrow-gmaps.files.gmaps-api-key.path;
  };

  services.opencrow.skills = [
    {
      name = "gmaps-cli";
      path = "${micsSkills}/skills/gmaps-cli";
    }
  ];

  services.opencrow.extraPackages = [ micsSkillsPkgs.gmaps-cli ];

  containers.opencrow.config.systemd.tmpfiles.rules = [
    "d /var/lib/opencrow/.config/gmaps-cli 0750 opencrow opencrow -"
    ''f /var/lib/opencrow/.config/gmaps-cli/config.json 0640 opencrow opencrow - {"api_key_command":"rbw get google-maps-api-key"}''
  ];
}
