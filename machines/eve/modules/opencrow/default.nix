{
  lib,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};

  cfg = config.services.opencrow;

  skillsDir = pkgs.linkFarm "opencrow-skills" (
    lib.mapAttrsToList (name: path: { inherit name path; }) cfg.skills
  );
in
{
  imports = [
    self.inputs.opencrow.nixosModules.default
    ./rbw.nix
    ./nostr.nix
    ./gitea.nix
    ./kagi.nix
    ./gmaps.nix
    ./mail.nix
    ./calendar.nix
    ./n8n.nix
  ];

  options.services.opencrow.skills = lib.mkOption {
    type = lib.types.attrsOf lib.types.path;
    default = { };
    description = "Skill directories (name → path) to expose to the agent via OPENCROW_PI_SKILLS_DIR.";
  };

  config = {

    # --- Pin opencrow uid/gid so host services (n8n) can share the group ---

    containers.opencrow.config.users.users.opencrow.uid = 2000;
    containers.opencrow.config.users.groups.opencrow.gid = 2000;
    users.groups.opencrow.gid = 2000;

    # /etc/localtime is bind-mounted from the host (UTC) in nspawn containers,
    # so we rely on /etc/timezone and TZ env var instead.
    containers.opencrow.config.environment.etc."timezone".text = "Europe/Berlin\n";

    containers.opencrow.config.systemd.tmpfiles.rules = [
      "d /var/lib/opencrow/.config 0750 opencrow opencrow -"
    ];

    # --- Skills ---

    services.opencrow.skills = lib.genAttrs [
      "context7-cli"
      "db-cli"
      "pexpect-cli"
      "weather-cli"
    ] (name: "${micsSkills}/skills/${name}");

    # --- Service ---

    services.opencrow = {
      enable = true;
      piPackage = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
      environment = {
        TZ = "Europe/Berlin";
        OPENCROW_SOUL_FILE = "${./soul.md}";
        OPENCROW_PI_SKILLS_DIR = "${skillsDir}";
        OPENCROW_LOG_LEVEL = "debug";
        OPENCROW_PI_PROVIDER = "anthropic";
        OPENCROW_PI_MODEL = "claude-sonnet-4-6";
      };
      extraPackages = [
        micsSkillsPkgs.context7-cli
        micsSkillsPkgs.db-cli
        micsSkillsPkgs.pexpect-cli
        micsSkillsPkgs.weather-cli
      ]
      ++ (with pkgs; [
        curl
        file
        git
        jq
        less
        procps
        python3
        tree
        util-linux
        w3m
        which
      ]);
    };
  };
}
