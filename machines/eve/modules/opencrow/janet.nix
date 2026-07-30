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
    #./morpheus.nix
    ./vllm-jack.nix
    ./openrouter.nix
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
    containers.opencrow.config.imports = [ ../agent-container.nix ];
    # Base tools come from the container profile (agent-container.nix).
    containers.opencrow.config.systemd.services.opencrow.path = [ "/run/current-system/sw" ];
    containers.opencrow.config.users.users.opencrow.uid = 2000;
    containers.opencrow.config.users.groups.opencrow.gid = 2000;
    users.groups.opencrow.gid = 2000;
    # Host-side mirror of the container user so the nix daemon accepts
    # connections through the bind-mounted daemon socket.
    users.users.opencrow = {
      isSystemUser = true;
      group = "opencrow";
      uid = 2000;
    };
    nix.settings.extra-allowed-users = [ "opencrow" ];

    containers.opencrow.config.systemd.tmpfiles.rules = [
      "d /var/lib/opencrow/.config 0750 opencrow opencrow -"
    ];

    services.opencrow = {
      enable = true;

      skills = lib.genAttrs [
        "db-cli"
        "n8n-cli"
        "weather-cli"
      ] (name: "${micsSkills}/${name}/skill");

      environment = {
        OPENCROW_SOUL_FILE = "${./soul.md}";
        OPENCROW_LOG_LEVEL = "debug";
        OPENCROW_PI_PROVIDER = "jack";
        OPENCROW_PI_MODEL = "qwen3-30b-a3b-instruct";
      };

      extraPackages = [
        micsSkillsPkgs.db-cli
        micsSkillsPkgs.n8n-cli
        micsSkillsPkgs.weather-cli
      ];
    };
  };
}
