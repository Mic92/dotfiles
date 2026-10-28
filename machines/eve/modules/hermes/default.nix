{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  # Upstream pins exact patch versions (prompt-toolkit, markdown) that lag
  # behind our nixpkgs.
  hermesPkg =
    self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent.overridePythonAttrs
      { dontCheckRuntimeDeps = true; };
  stateDir = "/var/lib/hermes";
  gen = config.clan.core.vars.generators.hermes;
in
{
  # Slack credentials for the Hermes gateway.
  clan.core.vars.generators.hermes = {
    files.slack-bot-token.secret = true;
    files.slack-app-token.secret = true;
    files.anthropic-api-key.secret = true;

    prompts.slack-bot-token.description = "Slack bot token (xoxb-…) for the Hermes app";
    prompts.slack-app-token.description = "Slack app-level token (xapp-…) with connections:write";
    prompts.anthropic-api-key.description = "Anthropic token";

    script = ''
      cp "$prompts/slack-bot-token" "$out/slack-bot-token"
      cp "$prompts/slack-app-token" "$out/slack-app-token"
      cp "$prompts/anthropic-api-key" "$out/anthropic-api-key"
    '';
  };

  users.users.hermes = {
    isSystemUser = true;
    group = "hermes";
    uid = 2001;
  };
  users.groups.hermes.gid = 2001;
  nix.settings.extra-allowed-users = [ "hermes" ];

  clan.core.vars.generators.hermes-ldap = {
    files.password = { };
    runtimeInputs = [
      pkgs.coreutils
      pkgs.openssl
    ];
    script = ''openssl rand -hex 32 | tr -d '\n' > "$out/password"'';
  };

  services.lldap.ensureUsers.hermes = {
    email = "hermes@thalheim.io";
    passwordFile = config.clan.core.vars.generators.hermes-ldap.files.password.path;
    groups = [ "lldap_strict_readonly" ];
  };

  services.postgresql.ensureUsers = [ { name = "hermes"; } ];
  systemd.services.postgresql-setup.script = lib.mkAfter ''
    psql -tAc 'GRANT pg_read_all_data TO "hermes"'
  '';

  systemd.tmpfiles.rules = [
    "d ${stateDir} 0750 - - -"
  ];

  containers.hermes = {
    autoStart = true;
    privateNetwork = false;

    bindMounts.${stateDir} = {
      hostPath = stateDir;
      isReadOnly = false;
    };
    bindMounts."/run/postgresql".isReadOnly = true;

    extraFlags = [
      "--load-credential=slack-bot-token:${gen.files.slack-bot-token.path}"
      "--load-credential=slack-app-token:${gen.files.slack-app-token.path}"
      "--load-credential=anthropic-api-key:${gen.files.anthropic-api-key.path}"
      "--load-credential=ldap-password:${config.clan.core.vars.generators.hermes-ldap.files.password.path}"
      "--load-credential=inference-api-key:${config.clan.core.vars.generators.litellm.files.master-key.path}"
    ];

    config = _: {
      imports = [ ../agent-container.nix ];

      system.stateVersion = "25.05";

      # Copy the host's retiolum entries so jack.r resolves inside.
      networking.extraHosts = config.networking.extraHosts;

      users.users.hermes = {
        isSystemUser = true;
        group = "hermes";
        uid = 2001;
        home = stateDir;
      };
      users.groups.hermes.gid = 2001;

      environment.systemPackages = [
        hermesPkg
        pkgs.radicle-node
        config.services.postgresql.package
        pkgs.openldap
      ];

      environment.variables = {
        PGHOST = "/run/postgresql";
        PGDATABASE = "postgres";
      };

      # `ldapsearch -y /run/credentials/hermes.service/ldap-password` binds as hermes.
      environment.etc."openldap/ldap.conf".text = ''
        URI ldap://localhost:3890
        BASE dc=eve
        BINDDN uid=hermes,ou=people,dc=eve
      '';

      programs.git = {
        enable = true;
        config.user = {
          name = "Hermes Bot";
          email = "hermes@thalheim.io";
        };
      };

      environment.variables.XDG_RUNTIME_DIR = "/run/user/2001";

      systemd.services.radicle-node = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        environment = {
          HOME = stateDir;
          XDG_RUNTIME_DIR = "/run/user/2001";
        };
        serviceConfig = {
          User = "hermes";
          Group = "hermes";
          ExecStart = "${pkgs.radicle-node}/bin/radicle-node";
          Restart = "on-failure";
        };
      };

      systemd.tmpfiles.rules =
        let
          hermesConfig = pkgs.writers.writeYAML "hermes-config.yaml" {
            model.model = "qwen3.8-27b";
            provider_routing = {
              data_collection = "deny";
              only = [ "venice" ];
            };
          };
        in
        [
          "d ${stateDir} 0750 hermes hermes -"
          "d ${stateDir}/.hermes 0750 hermes hermes -"
          "d /run/user/2001 0700 hermes hermes -"
          "L+ ${stateDir}/.hermes/config.yaml - - - - ${hermesConfig}"
        ];

      systemd.services.hermes = {
        description = "Hermes Agent Slack gateway";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];

        # Base tools come from the container profile (agent-container.nix).
        path = [
          hermesPkg
          "/run/current-system/sw"
        ];

        environment = {
          TZ = "Europe/Berlin";
          NIX_REMOTE = "daemon";
          HOME = stateDir;
          HERMES_HOME = "${stateDir}/.hermes";
          XDG_RUNTIME_DIR = "/run/user/2001";
          HERMES_INFERENCE_PROVIDER = "vllm";
          # LiteLLM on the host (see ../litellm.nix): p0 with jack fallback.
          CUSTOM_BASE_URL = "http://[::1]:4000/v1";
          # hermes only sends OPENAI_API_KEY to the host named here.
          OPENAI_BASE_URL = "http://[::1]:4000/v1";
          HERMES_MODEL = "qwen3.8-27b";
          SLACK_ALLOWED_USERS = "U02TAKGUGF4";
          LDAP_PASSWORD_FILE = "%d/ldap-password";
        };

        serviceConfig = {
          User = "hermes";
          Group = "hermes";
          WorkingDirectory = stateDir;
          StateDirectory = "hermes";
          ImportCredential = [
            "slack-bot-token"
            "slack-app-token"
            "anthropic-api-key"
            "inference-api-key"
            "ldap-password"
          ];
          Restart = "on-failure";
          RestartSec = 30;
          ExecStart = pkgs.writeShellScript "hermes-gateway" ''
            set -euo pipefail
            SLACK_BOT_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-bot-token")
            SLACK_APP_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-app-token")
            # hermes expects OAuth/setup tokens in ANTHROPIC_TOKEN (not ANTHROPIC_AUTH_TOKEN)
            ANTHROPIC_TOKEN=$(< "$CREDENTIALS_DIRECTORY/anthropic-api-key")
            OPENAI_API_KEY=$(< "$CREDENTIALS_DIRECTORY/inference-api-key")
            export SLACK_BOT_TOKEN SLACK_APP_TOKEN ANTHROPIC_TOKEN OPENAI_API_KEY
            exec ${lib.getExe hermesPkg} gateway run
          '';
        };
      };
    };
  };
}
