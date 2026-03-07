{
  config,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};

  # Only include skills for tools we actually install in extraPackages.
  skillsDir = pkgs.linkFarm "opencrow-skills" (
    map
      (name: {
        inherit name;
        path = "${micsSkills}/skills/${name}";
      })
      [
        "context7-cli"
        "db-cli"
        "gmaps-cli"
        "kagi-search"
        "pexpect-cli"
        "weather-cli"
      ]
    ++ [
      {
        name = "calendar";
        path = ./skills/calendar;
      }
      {
        name = "email";
        path = ./skills/email;
      }
      {
        name = "n8n-workflows";
        path = ./skills/n8n-workflows;
      }
    ]
  );

  # Mock rbw that returns secrets from systemd credential files.
  # Maps "rbw get <args>" to credential file names.
  credDir = "/run/credentials/opencrow.service";
  rbwEntries = {
    "Eve" = "nextcloud-thalheim-password";
    "nextcloud.clan.lol Mic92" = "nextcloud-clan-password";
    "kagi-session-link" = "kagi-session-token";
    "google-maps-api-key" = "gmaps-api-key";
    "n8n-api-jwt" = "n8n-api-jwt";
  };
  mockRbw = pkgs.writeShellScriptBin "rbw" (
    ''
      if [ "$1" != "get" ]; then
        echo "mock rbw: only 'get' is supported" >&2
        exit 1
      fi
      shift
      key="$*"
      case "$key" in
    ''
    + pkgs.lib.concatStrings (
      pkgs.lib.mapAttrsToList (args: credFile: ''
        ${pkgs.lib.escapeShellArg args})
          cat "${credDir}/${credFile}"
          ;;
      '') rbwEntries
    )
    + ''
        *)
          echo "mock rbw: unknown entry: $key" >&2
          exit 1
          ;;
      esac
    ''
  );
in
{
  imports = [
    self.inputs.opencrow.nixosModules.default
    ./mail.nix
    ./calendar.nix
    ./n8n.nix
  ];

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

  clan.core.vars.generators.opencrow-ssh = {
    files.ssh-private-key.secret = true;
    files.ssh-public-key.secret = false;

    runtimeInputs = with pkgs; [ openssh ];

    script = ''
      ssh-keygen -t ed25519 -N "" -f "$out/ssh-private-key" -C "opencrow@eve"
      ssh-keygen -y -f "$out/ssh-private-key" > "$out/ssh-public-key"
    '';
  };

  clan.core.vars.generators.opencrow-gitea = {
    files.gitea-password.secret = true;

    runtimeInputs = with pkgs; [ openssl ];

    script = ''
      openssl rand -base64 32 | tr -d '\n' > "$out/gitea-password"
    '';
  };

  clan.core.vars.generators.opencrow-skills = {
    files.kagi-session-token.secret = true;
    files.gmaps-api-key.secret = true;

    prompts.kagi-session-token.description = "Kagi session token for web search";
    prompts.gmaps-api-key.description = "Google Maps API key for gmaps-cli";

    script = ''
      cp "$prompts/kagi-session-token" "$out/kagi-session-token"
      cp "$prompts/gmaps-api-key" "$out/gmaps-api-key"
    '';
  };

  # --- Pin opencrow uid/gid so host services (n8n) can share the group ---

  containers.opencrow.config.users.users.opencrow.uid = 2000;
  containers.opencrow.config.users.groups.opencrow.gid = 2000;
  users.groups.opencrow.gid = 2000;

  # --- SSH keys ---

  containers.opencrow.bindMounts."/run/opencrow-ssh/id_ed25519" = {
    hostPath = config.clan.core.vars.generators.opencrow-ssh.files.ssh-private-key.path;
    isReadOnly = true;
  };
  containers.opencrow.bindMounts."/run/opencrow-ssh/id_ed25519.pub" = {
    hostPath = config.clan.core.vars.generators.opencrow-ssh.files.ssh-public-key.path;
    isReadOnly = true;
  };

  systemd.tmpfiles.rules = [
    "d /run/opencrow-ssh 0700 root root -"
    "f /run/opencrow-ssh/id_ed25519 0600 root root -"
    "f /run/opencrow-ssh/id_ed25519.pub 0644 root root -"
  ];

  # /etc/localtime is bind-mounted from the host (UTC) in nspawn containers,
  # so we rely on /etc/timezone and TZ env var instead.
  containers.opencrow.config.environment.etc."timezone".text = "Europe/Berlin\n";

  # Copy bind-mounted SSH keys (root-owned) into ~/.ssh with opencrow
  # ownership so all SSH-based tools (git, ssh) work without wrapper hacks.
  containers.opencrow.config.systemd.services.opencrow-ssh-keys = {
    description = "Install opencrow SSH keys";
    wantedBy = [ "multi-user.target" ];
    before = [
      "opencrow.service"
      "opencrow-clone-repos.service"
    ];
    serviceConfig.Type = "oneshot";
    script = ''
      install -d -m 0700 -o opencrow -g opencrow /var/lib/opencrow/.ssh
      install -m 0600 -o opencrow -g opencrow /run/opencrow-ssh/id_ed25519 /var/lib/opencrow/.ssh/id_ed25519
      install -m 0644 -o opencrow -g opencrow /run/opencrow-ssh/id_ed25519.pub /var/lib/opencrow/.ssh/id_ed25519.pub
    '';
  };

  containers.opencrow.config.systemd.tmpfiles.rules = [
    "d /var/lib/opencrow/.config 0750 opencrow opencrow -"
    "d /var/lib/opencrow/.config/kagi 0750 opencrow opencrow -"
    ''f /var/lib/opencrow/.config/kagi/config.json 0640 opencrow opencrow - {"password_command":"rbw get kagi-session-link"}''
    "d /var/lib/opencrow/.config/gmaps-cli 0750 opencrow opencrow -"
    ''f /var/lib/opencrow/.config/gmaps-cli/config.json 0640 opencrow opencrow - {"api_key_command":"rbw get google-maps-api-key"}''
    ''f /var/lib/opencrow/.gitconfig 0644 opencrow opencrow - [user]\n\tname = Janet\n\temail = janet@thalheim.io''
  ];

  # --- Service ---

  services.opencrow = {
    enable = true;
    piPackage = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;
    credentialFiles = {
      "nostr-private-key" = config.clan.core.vars.generators.opencrow.files.nostr-private-key.path;
      "kagi-session-token" =
        config.clan.core.vars.generators.opencrow-skills.files.kagi-session-token.path;
      "gmaps-api-key" = config.clan.core.vars.generators.opencrow-skills.files.gmaps-api-key.path;
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
      mockRbw
      micsSkillsPkgs.context7-cli
      micsSkillsPkgs.db-cli
      micsSkillsPkgs.gmaps-cli
      micsSkillsPkgs.kagi-search
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
}
