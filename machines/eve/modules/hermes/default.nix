{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  hermesPkg = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent;
  stateDir = "/var/lib/hermes";
  gen = config.clan.core.vars.generators.hermes;
in
{
  # Slack + Anthropic credentials for the Hermes gateway.
  clan.core.vars.generators.hermes = {
    files.slack-bot-token.secret = true;
    files.slack-app-token.secret = true;
    files.anthropic-token.secret = true;

    prompts.slack-bot-token.description = "Slack bot token (xoxb-…) for the Hermes app";
    prompts.slack-app-token.description = "Slack app-level token (xapp-…) with connections:write";
    prompts.anthropic-token.description = "Anthropic OAuth setup token (sk-ant-oat…) from `claude setup-token`";

    script = ''
      cp "$prompts/slack-bot-token" "$out/slack-bot-token"
      cp "$prompts/slack-app-token" "$out/slack-app-token"
      cp "$prompts/anthropic-token" "$out/anthropic-token"
    '';
  };

  # The hermes user only exists inside the container; create the bind-mount
  # source as root here, ownership is fixed up from the inside.
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

    extraFlags = [
      "--load-credential=slack-bot-token:${gen.files.slack-bot-token.path}"
      "--load-credential=slack-app-token:${gen.files.slack-app-token.path}"
      "--load-credential=anthropic-token:${gen.files.anthropic-token.path}"
    ];

    config = _: {
      system.stateVersion = "25.05";

      users.users.hermes = {
        isSystemUser = true;
        group = "hermes";
        uid = 2001;
        home = stateDir;
      };
      users.groups.hermes.gid = 2001;

      environment.etc."timezone".text = "Europe/Berlin\n";

      systemd.tmpfiles.rules = [
        "d ${stateDir} 0750 hermes hermes -"
      ];

      systemd.services.hermes = {
        description = "Hermes Agent Slack gateway";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];

        path = [
          hermesPkg
        ]
        ++ (with pkgs; [
          bash
          coreutils
          curl
          fd
          file
          findutils
          git
          gnugrep
          gnused
          gnutar
          gzip
          jq
          openssh
          procps
          ripgrep
          unzip
          util-linux
          which
          xz
        ]);

        environment = {
          TZ = "Europe/Berlin";
          HOME = stateDir;
          HERMES_HOME = "${stateDir}/.hermes";
          # Anthropic subscription (setup token) instead of API key; the token
          # itself comes in via ANTHROPIC_TOKEN from systemd credentials.
          HERMES_INFERENCE_PROVIDER = "anthropic";
          HERMES_INFERENCE_MODEL = "claude-opus-4-6";
          SLACK_ALLOWED_USERS = "U02TAKGUGF4";
        };

        serviceConfig = {
          User = "hermes";
          Group = "hermes";
          WorkingDirectory = stateDir;
          StateDirectory = "hermes";
          ImportCredential = [
            "slack-bot-token"
            "slack-app-token"
            "anthropic-token"
          ];
          Restart = "on-failure";
          RestartSec = 30;
          ExecStart = pkgs.writeShellScript "hermes-gateway" ''
            set -euo pipefail
            SLACK_BOT_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-bot-token")
            SLACK_APP_TOKEN=$(< "$CREDENTIALS_DIRECTORY/slack-app-token")
            ANTHROPIC_TOKEN=$(< "$CREDENTIALS_DIRECTORY/anthropic-token")
            export SLACK_BOT_TOKEN SLACK_APP_TOKEN ANTHROPIC_TOKEN
            exec ${lib.getExe hermesPkg} gateway run
          '';
        };
      };
    };
  };
}
