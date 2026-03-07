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

  skillsDir = pkgs.linkFarm "opencrow-skills" cfg.skills;
in
{
  imports = [
    self.inputs.opencrow.nixosModules.default
    ./rbw.nix
    ./gitea.nix
    ./kagi.nix
    ./gmaps.nix
    ./mail.nix
    ./calendar.nix
    ./n8n.nix
  ];

  options.services.opencrow.skills = lib.mkOption {
    type = lib.types.listOf (
      lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Skill directory name.";
          };
          path = lib.mkOption {
            type = lib.types.path;
            description = "Path to skill directory (must contain SKILL.md).";
          };
        };
      }
    );
    default = [ ];
    description = "Skill directories to expose to the agent via OPENCROW_PI_SKILLS_DIR.";
  };

  config = {

    # --- Identity secrets ---

    clan.core.vars.generators.opencrow = {
      files.nostr-private-key.secret = true;
      files.nostr-public-key.secret = false;

      runtimeInputs = with pkgs; [ nak ];

      script = ''
        sk=$(nak key generate)
        pk=$(nak key public "$sk")
        echo -n "$sk" > "$out/nostr-private-key"
        echo -n "$pk" > "$out/nostr-public-key"
      '';
    };

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

    services.opencrow.skills =
      map
        (name: {
          inherit name;
          path = "${micsSkills}/skills/${name}";
        })
        [
          "context7-cli"
          "db-cli"
          "pexpect-cli"
          "weather-cli"
        ];

    # --- Service ---

    services.opencrow = {
      enable = true;
      piPackage = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
      credentialFiles = {
        "nostr-private-key" = config.clan.core.vars.generators.opencrow.files.nostr-private-key.path;
      };
      environment = {
        OPENCROW_BACKEND = "nostr";
        OPENCROW_NOSTR_PRIVATE_KEY_FILE = "%d/nostr-private-key";
        OPENCROW_NOSTR_RELAYS = "wss://nostr.0cx.de,wss://relay.damus.io,wss://nos.lol";
        OPENCROW_NOSTR_DM_RELAYS = "wss://relay.damus.io,wss://nos.lol";
        OPENCROW_NOSTR_BLOSSOM_SERVERS = "https://blossom.nostr.build";
        OPENCROW_NOSTR_ALLOWED_USERS = "npub10yt4rh4g5t5kd47x7w8dpwqq7s228c53xacjqxvxjwu0kes3kzvsynqfu8";
        TZ = "Europe/Berlin";
        OPENCROW_NOSTR_NAME = "janet";
        OPENCROW_NOSTR_DISPLAY_NAME = "Janet";
        OPENCROW_NOSTR_ABOUT = "Not a robot. Not a girl. I'm Janet! 👋 An anthropomorphized vessel of knowledge, here to help.";
        OPENCROW_NOSTR_PICTURE = "https://robohash.org/96dc8a8cb0c28bdd113c1f6e350abd6014c69369cbd618c3b8cd4d1326bf7e37?set=set4&size=256x256";
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
