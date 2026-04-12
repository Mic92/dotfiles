{
  lib,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  # Janet is the primary, broad-scope assistant. She uses the opencrow
  # "default" instance so all the domain modules below that target
  # services.opencrow.* attach to her without further plumbing.
  imports = [
    ./rbw.nix
    ./morpheus.nix
    ./nostr.nix
    ./gitea.nix
    ./kagi.nix
    ./gmaps.nix
    ./mail.nix
    ./calendar.nix
    ./n8n.nix
    ./paperless.nix
  ];

  config = {
    containers.opencrow.config.users.users.opencrow.uid = 2000;
    containers.opencrow.config.users.groups.opencrow.gid = 2000;
    users.groups.opencrow.gid = 2000;

    containers.opencrow.config.environment.etc."timezone".text = "Europe/Berlin\n";

    containers.opencrow.config.systemd.tmpfiles.rules = [
      "d /var/lib/opencrow/.config 0750 opencrow opencrow -"
    ];

    services.opencrow = {
      enable = true;

      skills = lib.genAttrs [
        "db-cli"
        "n8n-cli"
        "weather-cli"
      ] (name: "${micsSkills}/skills/${name}");

      environment = {
        OPENCROW_SOUL_FILE = "${./soul.md}";
        OPENCROW_LOG_LEVEL = "debug";
        OPENCROW_PI_PROVIDER = "morpheus";
        # Use the unfiltered alias; see morpheus.nix for why the base
        # id is currently unsuitable for agent loops.
        OPENCROW_PI_MODEL = "qwen-35-35b-coding";
      };

      extraPackages = [
        micsSkillsPkgs.db-cli
        micsSkillsPkgs.n8n-cli
        micsSkillsPkgs.weather-cli
      ];
    };
  };
}
