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
  clan.core.vars.generators.opencrow-kagi = {
    files.kagi-session-token.secret = true;

    prompts.kagi-session-token.description = "Kagi session token for web search";

    script = ''
      cp "$prompts/kagi-session-token" "$out/kagi-session-token"
    '';
  };

  services.opencrow.rbwEntries = {
    "kagi-session-link" = "kagi-session-token";
  };

  services.opencrow.credentialFiles = {
    "kagi-session-token" = config.clan.core.vars.generators.opencrow-kagi.files.kagi-session-token.path;
  };

  services.opencrow.skills = [
    {
      name = "kagi-search";
      path = "${micsSkills}/skills/kagi-search";
    }
  ];

  services.opencrow.extraPackages = [ micsSkillsPkgs.kagi-search ];

  containers.opencrow.config.systemd.tmpfiles.rules = [
    "d /var/lib/opencrow/.config/kagi 0750 opencrow opencrow -"
    ''f /var/lib/opencrow/.config/kagi/config.json 0640 opencrow opencrow - {"password_command":"rbw get kagi-session-link"}''
  ];
}
